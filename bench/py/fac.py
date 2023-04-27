def fac(n):
    if n < 2:
        return 1
    else:
        return n * fac(n - 1)

def test(n):
    for x in range(0, n):
        if x == n - 1:
            print(fac(5))
        else:
            fac(5)

test(450000)
