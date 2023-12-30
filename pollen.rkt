#lang racket

(require pollen/core)

(provide (all-defined-out))

(define (badge topic)
    `(li ([class "mr-1.5 mb-2"])
		(div ([class "flex items-center rounded-full bg-[#E28E78]/10 px-3 py-1 text-xs font-medium leading-5 text-[#E28E78]"]) ,topic)))

(define (->badges topics)
  (let ([top-list (map (curry string-trim #:left? #t) 
  					   (string-split topics ","))])
    `(ul ([class "flex flex-wrap"] [aria-label "Tools/Techniques used"])
         ,(for/splice ([topic top-list]) (badge topic)))))

(define (link text href)
    `(a ([class "font-medium text-zinc-200 hover:text-[#9BBEFF] focus-visible:text-[#9BBEFF]"] [target "_blank"] [rel "noreferrer"] [href ,href])
        ,text))

(define (footer-link text href)
    `(a ([class "font-medium text-zinc-400 hover:text-[#6E98E8] focus-visible:text-[#6E98E8]"] [target "_blank"] [rel "noreferrer"] [href ,href])
        ,text))

(define (relevant-link text href)
    `(li ([class "mr-4"])
            (a ([class "relative inline-flex items-center text-sm font-medium text-zinc-300 hover:text-[#9BBEFF] focus-visible:text-[#9BBEFF]"] [href ,href] [target "_blank"] [rel "noreferrer"])
                (svg ([xmlns "http://www.w3.org/2000/svg"] [viewBox "0 0 20 20"] [fill "currentColor"] [class "mr-1 h-3 w-3"] [aria-hidden "true"])
                        (path ([d "M12.232 4.232a2.5 2.5 0 013.536 3.536l-1.225 1.224a.75.75 0 001.061 1.06l1.224-1.224a4 4 0 00-5.656-5.656l-3 3a4 4 0 00.225 5.865.75.75 0 00.977-1.138 2.5 2.5 0 01-.142-3.667l3-3z"]))
                        (path ([d "M11.603 7.963a.75.75 0 00-.977 1.138 2.5 2.5 0 01.142 3.667l-3 3a2.5 2.5 0 01-3.536-3.536l1.225-1.224a.75.75 0 00-1.061-1.06l-1.224 1.224a4 4 0 105.656 5.656l3-3a4 4 0 00-.225-5.865z"])))
                (span ,text))))

(define/match (relevant-links links)
  [('()) ""]
  [(links) (let ([names (map (λ (pair) (car pair)) links)]
        [hrefs (map (λ (pair) (cdr pair)) links)])
    `(ul ([class "mb-2 flex flex-wrap"] [aria-label "Related links"])
         ,(for/splice ([name names] [href hrefs]) (relevant-link name href))))])


(define (card #:title [title ""] #:position [pos ""] #:organization [org ""] #:link [link ""]
              #:description [description ""] #:links [links '()] #:topics [topics ""]
              #:image-src [image-src ""]
              . text)
    `(li ([class "mb-12"])
        (div ([class "group relative grid pb-1 transition-all sm:grid-cols-8 sm:gap-8 md:gap-4 lg:hover:!opacity-100 lg:group-hover/list:opacity-50"])
            (div ([class "absolute -inset-x-4 -inset-y-4 z-0 rounded-md transition lg:-inset-x-6 lg:block lg:group-hover:bg-zinc-800/50 lg:group-hover:shadow-[inset_0_1px_0_0_rgba(148,163,184,0.1)] group-hover:drop-shadow-lg"]))
            ,(when/splice (not (equal? description ""))
                          `(header ([class "z-10 mb-2 mt-1 text-xs font-semibold uppercase tracking-wide text-zinc-500 sm:col-span-2"]) ,description))
            ,(when/splice (not (equal? image-src ""))
                          `(img ([class "w-20 mb-2 rounded hover:drop-shadow sm:order-1 sm:col-span-2 sm:translate-y-1"] [loading "lazy"] [data-nimg "1"] [width "200"] [height "48"] [src ,image-src])))
            (div ([class "z-10 sm:col-span-6 order-0 sm:order-2"])
                ,(when/splice (not (equal? title ""))
                  `(h3 ([class "mb-2 font-medium leading-snug text-zinc-200"])
                    (div (a ([class "inline-flex items-baseline font-medium leading-tight text-zinc-200 hover:text-[#9BBEFF] focus-visible:text-[#9BBEFF] group/link text-base"] [href ,link] [target "_blank"] [rel "noreferrer"])
                            (span ([class "absolute -inset-x-4 -inset-y-2.5 hidden rounded md:-inset-x-6 md:-inset-y-4 lg:block"]))
                            (span ,title
                                  ,(when/splice (not (equal? org ""))
                                    `(@ 
                                        (span ([class "px-1"]) "|")
                                        (span ([class "inline-block"]) ,org))))))
                    (div ([class "text-zinc-500"])
                         (span ,pos))))
                ,(when/splice (pair? text) 
                  `(div ([class "mb-4 text-sm leading-normal"])
                    ,@text))
                ,(relevant-links links)
                ,(->badges topics)))))

(define (hrule)
  `(section ([id ,(string-downcase "delimiter")] [class "group/section"])
			 (div ([class "sticky top-0 z-20 -mx-6 mb-4 w-screen px-5 pb-5 backdrop-blur md:-mx-12 md:px-12 lg:relative lg:px-0 lg:top-auto lg:mx-auto lg:w-full"])
			 	  (h2 ([class "border-t-2 border-solid border-[#DC755C] lg:border-transparent lg:text-sm lg:text-zinc-200 lg:group-hover/section:text-base lg:transition-all lg:duration-500 lg:ease-in-out lg:group-hover/section:text-[#dc755c] lg:group-hover/section:border-[#DC755C]"])
				      ""))))

(define (section-title title)
  `(h2 ([class "pt-3 px-1 text-base font-medium text-[#dc755c] uppercase tracking-[2px] border-t-2 border-solid border-[#DC755C] lg:border-transparent lg:text-sm lg:text-zinc-200 lg:group-hover/section:text-base lg:transition-all lg:duration-500 lg:ease-in-out lg:group-hover/section:text-[#dc755c] lg:group-hover/section:border-[#DC755C]"])
				      ,title))

(define (section title aria-label . content)
    `(section ([id ,(string-downcase title)] [class "group/section mb-16 scroll-mt-16 md:mb-24 lg:mb-24 lg:scroll-mt-24"] [aria-label ,aria-label])
			 (div ([class "sticky top-0 z-20 -mx-6 mb-4 w-screen px-5 py-5 backdrop-blur md:-mx-12 md:px-12 lg:relative lg:px-0 lg:top-auto lg:mx-auto lg:w-full"])
			 	  ,(section-title title))
			 (div 
			 	  (ol ([class "group/list"])
				     ,@content
				))))