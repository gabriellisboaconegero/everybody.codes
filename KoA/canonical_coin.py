from math import ceil

def check(coins : list[int]):

    coins.sort()

    if coins[0] != 1:
        return False

    if len(coins) <= 2: # Trivial case.
        return True

    def greedy(x, idx):
        cnt = 0
        for i in reversed(range(idx + 1)):
            if x == 0:
                return cnt
            while(coins[i] <= x):
                x -= coins[i]
                cnt += 1

        return cnt

    for i in range(2, len(coins)):
        n = ceil(coins[i]/coins[i - 1])
        m = coins[i-1]*n - coins[i]
        if greedy(m, i - 2) >= n: # Performing greedy considering since i is Ck, i - 1 is Ck-1.
            return False

    return True

print(check([1, 3, 5, 10]))
print(check([10107,16432,10899,10000,15443,10422,10099,14671,16719,10059]))