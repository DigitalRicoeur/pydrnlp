#lang _-exp racket

(require pydrnlp)

(define args
  (list (tokenize-arg 'en 1 ƒstring-append{
 Turning and turning in the widening gyre
 The falcon cannot hear the falconer;
 Things fall apart; the centre cannot hold;
 Mere anarchy is loosed upon the world,
 The blood-dimmed tide is loosed, and everywhere
 The ceremony of innocence is drowned;
 The best lack all conviction, while the worst
 Are full of passionate intensity.
 })
        (tokenize-arg 'en 2 ƒstring-append{
 To use the Python functionality, you must have a conda executable
 in your PATH for the Conda package manager.
 The recommended way to obtain this is by installing the Miniconda
 distribution, though the larger Anaconda distribution should work as well.
 As long as you have conda installed, Racket will automatically take care
 of the details of creating or updating the virtual environment and managing
 Python dependencies.
 })
        (tokenize-arg 'fr 3 ƒstring-append{
 Mon cœur se recommande à vous
 Tout plein d'ennui et de martyre ;
 Au moins en dépit des jaloux
 Faites qu'adieu vous puisse dire !
 Ma bouche qui savait sourire
 Et conter propos gracieux
 Ne fait maintenant que maudire
 Ceux qui m'ont banni de vos yeux.
 })))

(define cust
  (make-custodian))

(define t
  (parameterize ([current-custodian cust])
    (launch-tokenizer #:quiet? #f)))

(let ([lst (custodian-managed-list cust (current-custodian))])
  (values lst
          (custodian-managed-list (car (filter custodian? lst))
                                  cust)))

(tokenizer-revision t)

(for ([arg (in-list args)])
  (pretty-print (tokenizer-tokenize t (list arg))))

(tokenizer-tokenize t args)

(define extra
  (list (tokenize-arg 'en 99999 ƒstring-append{
 Climate change policy toppled the government in Australia on Friday.

 How much does that really matter?

 It is certain to keep Australia from meeting its emissions targets
 under the Paris climate agreement.

 It’s also a glimpse into what a potent political issue climate change
 and energy policy can be in a handful of countries with powerful fossil
 fuel lobbies, namely Australia, Canada and the United States.

 In Australia, the world’s largest exporter of coal, climate and energy
 policy have infused politics for a decade, helping to bring down both
 liberal and conservative lawmakers.

 This week, the failure to pass legislation that would have reined in
 greenhouse gas emissions precipitated Malcolm Turnbull’s ouster as
 prime minister. He was elbowed out by Scott Morrison, an ardent
 champion of the Australian coal industry who is known for having
 brought a lump of the stuff to Parliament.

 It could be a bellwether for next year’s Canadian elections, expected
 in October, in which Prime Minister Justin Trudeau faces a powerful
 challenge from politicians aligned with the country’s oil industry.
 Conservatives have pledged to undo Mr. Trudeau’s plans to put a price
 on carbon nationwide if they take power. At the provincial level,
 conservatives won a majority in Ontario after campaigning against
 the province’s newly enacted cap-and-trade program.                                      
 })))

(tokenizer-tokenize t extra)

