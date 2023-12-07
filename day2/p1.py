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
    game = spel.split(':')
    game_id = game[0].split(' ')[1]
    dragningar = game[1].split(';')
    for dragning in dragningar:
        händer = dragning.split(',')
        for hand in händer:
            antal = hand.strip().split(' ')[0]
            färg = hand.strip().split(' ')[1]
            if (int(antal) > int(gränser[färg])):
                return 0
    return int(game_id)

summa = 0
for spel in input:
    summa = summa + kolla_spel(spel)

print(summa)