# With Agnes

with open('input2.txt') as f:
    input = f.readlines() 

# Get sum of IDs of possible games.

gränser = {
    "red": "12",
    "green": "13",
    "blue": "14"
}

def kolla_spel(spel):
    största_antal = {
        "red": 0,
        "green": 0,
        "blue": 0
    }
    game = spel.split(':')
    dragningar = game[1].split(';')
    for dragning in dragningar:
        händer = dragning.split(',')
        for hand in händer:
            antal = hand.strip().split(' ')[0]
            färg = hand.strip().split(' ')[1]
            if (int(antal) > största_antal[färg]):
                största_antal[färg] = int(antal)

    return största_antal["red"] * största_antal["green"] * största_antal["blue"]

summa = 0
for spel in input:
    summa += kolla_spel(spel)

print(summa)