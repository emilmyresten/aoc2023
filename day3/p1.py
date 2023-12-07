# With Agnes

with open('input.txt') as f:
    input = f.readlines()

print(input)

def kolla_runtom(rad, kolumn):
    runtom = ""
    if rad > 0 and kolumn > 0:
        runtom += input[rad-1][kolumn-1]

    if rad > 0:
        runtom += input[rad-1][kolumn]

    if rad > 0 and kolumn < len(input[0]) - 1:
        runtom += input[rad-1][kolumn+1]

    if kolumn > 0:
        runtom += input[rad][kolumn-1]

    if kolumn < len(input[0]) - 1:
        runtom += input[rad][kolumn+1]
    
    if rad < len(input) - 1 and kolumn > 0:
        runtom += input[rad+1][kolumn-1]

    if rad < len(input) - 1 and kolumn < len(input[0]) - 1:
        runtom += input[rad+1][kolumn+1]

    if rad < len(input) - 1:
        runtom += input[rad+1][kolumn]

    symbols = list(filter(lambda m: not(m == '.' or m == '\n' or m.isdigit()), runtom))
    return len(symbols) > 0



part_numbers = []
for rad_nummer, rad in enumerate(input):
    nuvarande_tal = ""
    inkludera_tal = False
    for kolumn_nummer, kolumn in enumerate(rad):
        if (kolumn.isdigit()):
            nuvarande_tal += kolumn
            if (inkludera_tal == False):
                inkludera_tal = kolla_runtom(rad_nummer, kolumn_nummer)
        else:
            if inkludera_tal:
                part_numbers.append(int(nuvarande_tal))
                inkludera_tal = False
            nuvarande_tal = ""

print(sum(part_numbers))
