#!/bin/bash

echo "Voer dit script uit vanuit de tests-directory!"

counter=0

swipl -f util.plt
status=$?
((counter+=status))

swipl -f parser.plt
status=$?
((counter+=status))

swipl -f mover.plt
status=$?
((counter+=status))

swipl -f score.plt
status=$?
((counter+=status))

if [ "$counter" -eq 0 ]; then
    echo "Alle PLUnit-testen geslaagd!"
else
    echo -e "\033[0;31mPLUnit-testen gefaald!"
fi

echo "Simulatie-testen uitvoeren, dit kan lang duren..."
cd ../opgave/vsRandom/ 
chmod +x error_finder.sh
./error_finder.sh 1
