def solve(command):
    
    x = {0:0, 1:0} # x[0]: North(+ve)-South(-ve), x[1]: East(+ve)-West(-ve)
    a = 0          # angle, 0,1,2,3 (North, East, South, West)


    for c in command:
        if c == 'F':
            if a == 0:
                x[0] += 1
            elif a == 1:
                x[1] += 1
            elif a == 2:
                x[0] -= 1
            else:
                x[1] -= 1
        elif c == 'L':
            a = (a - 1) % 4
        elif c == 'R':
            a = (a + 1) % 4

    # Command count to return to (0,0) will be the Manhattan distance
    # plus any necessary rotations.
    d = sum(map(abs, x.values()))
        
    print('Final position:', x, ', Final angle:', a, ', dist:', d)

    # Additional number of rotation commands to add, dependent on  whether
    # point lies ON or OFF axis, and which direction a s pointing.
    on = {0:2, 1:1, 2:0, 3:1}
    off = {0:2, 1:2, 2:1, 3:1}

    # Reflection across x[0]
    if x[1] < 0:
        if a in (1,3):
            a = (a + 2) % 4
    # Reflection across x[1]
    if x[0] < 0:
        if a in (0,2):
            a = (a + 2) % 4

    # Rotate counter clockwise
    if x[0] == 0 and x[1] != 0:
        a = (a - 1) % 4
                    
    # Add rotation to command count

    # Origin
    if x[0] == 0 and x[1] == 0:
        return d
    # On axis
    elif x[0] == 0 or x[1] == 0:
        return d + on[a]
    # Off axis
    else:
        return d + off[a]

command = 'RFFFFLFFRFLFLFFFLFFR'
solve(command)
