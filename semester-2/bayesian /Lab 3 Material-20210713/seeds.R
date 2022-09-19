y <- c(10, 23, 23, 26, 17,   5, 53, 55, 32, 46, 10,
        8, 10,  8, 23, 0,    3, 22, 15, 32, 3)
n <- c(39, 62, 81, 51, 39,   6, 74, 72, 51, 79,
       13, 16, 30, 28, 45,   4, 12, 41, 30, 51, 7)

y/n

seed <- c(rep("O75", 11), rep("O73", 10))
seed

root <- c(rep("B", 5), rep("C", 6), rep("B", 5), rep("C", 5))
root

seed <- factor(seed, levels = c("O75", "O73"))
root <- as.factor(root)

plot(seed, y/n)
plot(root, y/n)

seedroot <- factor(paste(seed, root, sep="-"),
                   levels = c("O75-B", "O75-C", "O73-B", "O73-C"))
plot(seedroot, y/n)