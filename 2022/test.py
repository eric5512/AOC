def read_lines():
    with open("D:/Programas/GitHub/aoc/2022/inputs/test.txt") as file:
        text = file.read().split("\n")
    return [[int(j) for j in list(i)] for i in text]

input = read_lines()
transposed = [[input[j][i] for j in range(len(input[0]))] for i in range(len(input))]

count = 0
for i in range(1,len(input)-1):
    row = input[i]
    for j in range(1,len(input[0])-1):
        col = transposed[j]
        val = input[i][j]
        if max(row[:j]) >= val and max(row[j+1:]) >= val and max(col[:i]) >= val and max(col[i+1:]) >= val:
            print(i,j,'|',max(row[:j]),max(row[j+1:]),max(col[:i]),max(col[i+1:]),'|',val)
            count += 1

print(len(input)*len(input[0])-count)