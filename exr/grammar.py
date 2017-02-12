# The list we are generating (start with the initial string).
OUT = {"AA"}
ITERATIONNS = 4 # Strictly less than 5 steps = 4 iterations.

for i in range(ITERATIONNS-1): # -1 to count S->AA as the first step
    for val in set(OUT):
        # If modifications are necessary, remove the current value...
        if val.count("A") > 0:
            OUT.remove(val)

        # which will be replaced by the following updates.
        for k in range(val.count("A")):
            # Mark the k'th A with an X.
            tmp = val.replace("A", "X", k+1).replace("X", "A", k)

            OUT.add(tmp.replace("X", "AAA"))
            OUT.add(tmp.replace("X", "a"))
            OUT.add(tmp.replace("X", "bA"))
            OUT.add(tmp.replace("X", "Ab"))

# We need to remove any unfinished strings.
for val in list(OUT):
    if "A" in val:
        OUT.remove(val)

# Done: print the results.
print(OUT)
print(len(OUT))
