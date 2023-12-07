

with open('input.txt') as f:
    input = f.readlines()

def kolla_runtom(rad, kolumn):
    runtom = {}
    if rad > 0 and kolumn > 0:
        runtom[(rad-1, kolumn-1)] = input[rad-1][kolumn-1]

    if rad > 0:
        runtom[(rad-1, kolumn)] = input[rad-1][kolumn]

    if rad > 0 and kolumn < len(input[0]) - 1:
        runtom[(rad-1, kolumn+1)] = input[rad-1][kolumn+1]

    if kolumn > 0:
        runtom[(rad, kolumn-1)] = input[rad][kolumn-1]

    if kolumn < len(input[0]) - 1:
        runtom[(rad, kolumn+1)] = input[rad][kolumn+1]
    
    if rad < len(input) - 1 and kolumn > 0:
        runtom[(rad+1,kolumn-1)] = input[rad+1][kolumn-1]

    if rad < len(input) - 1 and kolumn < len(input[0]) - 1:
        runtom[(rad+1,kolumn+1)] = input[rad+1][kolumn+1]

    if rad < len(input) - 1:
        runtom[(rad+1,kolumn)] = input[rad+1][kolumn]

    return dict(filter(lambda kv: kv[1] == '*', runtom.items()))

part_numbers = []
gears = {}
for rad_nummer, rad in enumerate(input):
    nuvarande_tal = ""
    stars_for_number_set = set({})
    inkludera_tal = False
    for kolumn_nummer, kolumn in enumerate(rad):
        if (kolumn.isdigit()):
            nuvarande_tal += kolumn
            stars = list(kolla_runtom(rad_nummer, kolumn_nummer).keys())
            for s in stars:
                stars_for_number_set.add(s)
            if inkludera_tal == False:
                inkludera_tal = len(stars) > 0
        
        else:
            if inkludera_tal:
                for star in stars_for_number_set:
                    if star in gears:
                        gears[star] += [(int(nuvarande_tal))]
                    else:
                        gears[star] = [int(nuvarande_tal)]
                inkludera_tal = False
                stars_for_number_set = set({})
            nuvarande_tal = ""


print(sum(list(map(lambda el: el[0] * el[1], list(dict(filter(lambda kv: len(kv[1]) == 2, gears.items())).values())))))
