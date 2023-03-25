#lang at-exp rscript

(require bex/experiment/blame-trail-data)

(define (remove-duplicate-bts bts)
  (remove-duplicates bts
                     #:key (match-lambda [(blame-trail-summary mod-name index trail-id mutants)
                                          (list mod-name index trail-id (length mutants))])))

(define (remove-duplicate-bts-in! data-file)
  (define original-bts (file->list data-file))
  (define unique-bts (remove-duplicate-bts original-bts))
  (display-lines-to-file (map ~s unique-bts)
                         data-file
                         #:exists 'replace))

(main
 #:arguments ([flags data-files]
              #:args data-files)
 #:check [(andmap path-to-existant-file? data-files)
          @~a{Can't find some data files in input}]

 (for ([data-file (in-list data-files)])
   (remove-duplicate-bts-in! data-file)))
