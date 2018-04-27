.PHONY: all
all: all.rktd stopword-lemmas.rktd
	echo "Done."


stopword-lemmas.rktd: stopwordLemmas.py
	python3 stopwordLemmas.py > stopword-lemmas.rktd


all.rktd: combine.rkt \
ideology-and-utopia.json \
function_of_fiction_in_shaping_reality.json \
between_Hermeneutics_and_Semiotics.json \
sorrowsandtheMakingofLife-Stories.json \
husserl.json \
freud-and-philosophy.json \
freedom-and-nature.json \
memory_history_forgetting.json
	racket combine.rkt



%.json: ../../texts/plain-text/%.txt \
../pydrnlp/drtoken.py \
../pydrnlp/suspiciousToken.py
	python3 ../pydrnlp/suspiciousToken.py "$@" < "$<"

