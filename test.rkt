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

(define t
  (launch-tokenizer #:quiet? #f))

(tokenizer-revision t)

(for ([arg (in-list args)])
  (println (tokenizer-tokenize t (list arg))))

(tokenizer-tokenize t args)




