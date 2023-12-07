# With Agnes

with open('input1.txt') as fil:
    innehåll = fil.readlines()

# innehåll = ["three1sk4hnine"]

resultat = []

par = { "one": "1",
        "two": "2",
        "three": "3",
        "four": "4", 
        "five": "5", 
        "six": "6", 
        "seven": "7",
        "eight": "8", 
        "nine": "9" }

siffror_som_bokstäver = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

for rad in innehåll:
    siffror = []
    for index, bokstav in enumerate(rad):
        if bokstav.isnumeric():
            siffror.append(bokstav)
        else:
            for siffra in siffror_som_bokstäver:
                if rad[index:].startswith(siffra):
                    siffror.append(par[siffra])


    tal = int(siffror[0] + siffror[-1])
    resultat.append(tal)


print(sum(resultat))