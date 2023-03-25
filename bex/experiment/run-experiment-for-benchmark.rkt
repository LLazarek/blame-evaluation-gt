#lang at-exp rscript

(define-runtime-paths
  [mutant-factory.rkt "mutant-factory.rkt"]
  [configuration-outcomes-dir "../dbs/code-mutations/configuration-outcomes"]
  [default-benchmarks-dir "../../gtp-benchmarks/benchmarks"]
  [default-configs-dir "../configurables/configs"]
  [default-output-dir "../../experiment-output"])

(define default-cpus (~a 2))
(define default-log-level "info")
(define default-parity-check-mode "check")

(main
 #:arguments ([(hash-table ['benchmark-name       benchmark-name]
                           ['config-name          config-name]
                           ['cpus                 (app string->number cpus)]
                           ['outdir-path          outdir-path]
                           ['log-level            log-level]
                           ['benchmark-data-name  benchmark-data-name]
                           ['benchmarks-path      benchmarks-dir]
                           ['configs-path         configs-dir]
                           ['parity-check-mode    parity-mode])
               args]
              #:once-each
              [("-b" "--benchmark")
               'benchmark-name
               ("The benchmark to run the experiment on."
                "The actual benchmark will be found in the directory specified by `-B`."
                "Mandatory.")
               #:collect {"name" take-latest #f}
               #:mandatory]
              [("-c" "--config")
               'config-name
               ("The experiment configuration name to run."
                "The actual config will be found in the directory specified by `-C`."
                "Mandatory.")
               #:collect {"name" take-latest #f}
               #:mandatory]

              [("-j" "--cpus")
               'cpus
               ("How many CPUs to use for running the experiment."
                "The experiment speed of execution scales linearly with CPUs with no practical"
                "limit (>1000s), so make this as large as possible."
                "However, be sure to have at least 4GB of RAM available per CPU used."
                "In practice memory usage is much lower than that, but it may use that much."
                @~a{Default: @default-cpus})
               #:collect {"N" take-latest default-cpus}]
              [("-o" "--out-dir")
               'outdir-path
               ("Path to the directory in which to place/find experiment results."
                @~a{Default: @default-output-dir})
               #:collect {"path" take-latest default-output-dir}]

              [("-l" "--log-level")
               'log-level
               ("The experiment logging level."
                "Valid choices are: debug, info, warning, error"
                @~a{Default: @default-log-level})
               #:collect {"<debug | info | warning | error>"
                          take-latest
                          default-log-level}]

              [("-N" "--name")
               'benchmark-data-name
               ("The name of the data set to be produced."
                "Default: the benchmark name supplied with `-b`.")
               #:collect {"name" take-latest #f}]
              [("-B" "--benchmarks-dir")
               'benchmarks-path
               ("Path to the directory containing experiment benchmarks."
                @~a{Default: @default-benchmarks-dir})
               #:collect {"path" take-latest default-benchmarks-dir}]
              [("-C" "--configs-dir")
               'configs-path
               ("Path to the directory containing experiment configs."
                @~a{Default: @default-configs-dir})
               #:collect {"path" take-latest default-configs-dir}]

              [("-p" "--parity-check-mode")
               'parity-check-mode
               ("Configuration/scenario outcome checking mode."
                "Valid choices are: record, check"
                @~a{Default: @default-parity-check-mode})
               #:collect {"<record | check>" take-latest default-parity-check-mode}])
 #:check [(member log-level '("debug" "info" "warning" "error"))
          @~a{@log-level is not a valid log level.}]
 #:check [(member parity-mode '("record" "check"))
          @~a{@parity-mode is not a valid parity checking mode.}]
 #:check [(positive-integer? cpus)
          @~a{There must be at least one CPU.}]
 (define benchmark-path (simple-form-path (build-path benchmarks-dir benchmark-name)))
 (define config-path (build-path configs-dir (~a config-name ".rkt")))
 (for ([f (list benchmark-path
                config-path)])
   (unless (or (file-exists? f)
               (directory-exists? f))
     (raise-user-error 'run-experiment
                       @~a{Bad argument: @f does not exist})))

 (define racket (simple-form-path (find-system-path 'exec-file)))

 (unless (directory-exists? outdir-path)
   (make-directory* outdir-path))
 (parameterize ([current-directory outdir-path])
   (define destination (simple-form-path (or benchmark-data-name benchmark-name)))
   (define data-dir (build-path destination "data"))
   (make-directory* data-dir)
   (define log-out
     (open-output-file (build-path destination (~a benchmark-name ".log"))
                       #:exists 'append))
   (file-stream-buffer-mode log-out 'none)
   (parameterize ([current-output-port log-out]
                  [current-error-port  log-out])
     (call-with-extended-environment
      (hash "PLTSTDOUT" (~a log-level "@factory")
            "PLTSTDERR" "none")
      (thunk
       (system* racket
                (~a mutant-factory.rkt)
                "-b"
                (~a benchmark-path)
                "-o"
                (~a data-dir)
                "-c"
                (~a config-path)
                "-n"
                (~a cpus)

                "-e"
                (build-path-string destination "errs.log")
                "-l"
                (build-path-string destination (~a benchmark-name "-progress.log"))
                "-m"
                (build-path-string destination (~a benchmark-name "-metadata.rktd"))

                (match parity-mode
                  ["record" "-P"]
                  ["check"  "-p"])
                (build-path-string configuration-outcomes-dir (~a benchmark-name ".rktd"))
                "-k"))))
   (close-output-port log-out)))

