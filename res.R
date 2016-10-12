fr = matrix(c(0.5, 0, 0, 1, 1, 0.5, 0, 1, 1, 1, 0.5, 1, 0, 0, 0, 0.5), 
            nrow = 4, ncol = 4, byrow  = TRUE)
rownames(fr) = c('NoMM', 'WP', 'TW', 'NoMY')
colnames(fr) = c('NoMM', 'WP', 'TW', 'NoMY')
c = rowSums(fr)
r = matrix(0, 4, 4)
m = 4 
for (i in 1:4) {
    for (j in 1:4) {
        r[i, j] = (c[i] - c[j])/ (2 * m) + 0.5
    }
}
rownames(r) = c('NoMM', 'WP', 'TW', 'NoMY')
colnames(r) = c('NoMM', 'WP', 'TW', 'NoMY')

d = rowSums(r)
s = matrix(0, 4, 4)
for (i in 1:4) {
    for (j in 1:4) {
        s[i ,j] = r[i ,j]/(d[j])
    }
}
colnames(s) = c('NoMM', 'WP', 'TW', 'NoMY')
rownames(s) = c('NoMM', 'WP', 'TW', 'NoMY')
k = apply(s, 1, prod) ** (1/4)
w = matrix(0, 4, 4)
for (i in 1:4) {
    w[i, ] = k[i]/sum(k)
}
colnames(w) = c('NoMM', 'WP', 'TW', 'NoMY')
rownames(w) = c('NoMM', 'WP', 'TW', 'NoMY')
weight = c(0, 0, 0, 0)
for (i in 1:4) {
    weight[i] = w[i, i]
}

## win percentage
url = "~/Google Drive/4893/independent research/data/manager.txt"
manager <- read.csv(url)
manager['ID'] = seq.int(from = 1, to = 179, by = 1)
manager[is.na(manager)] = 0
manager = as.data.frame(manager)

## manager of month
url = "~/Google Drive/4893/independent research/data/manager_mon.txt"
manager_mon = read.csv(url, 
                       header = FALSE, 
                       dec = ",")
colnames(manager_mon) = c('MANAGER', 'NoMM','ID')
manager_mon = as.data.frame(manager_mon)

## title won
url = "~/Google Drive/4893/independent research/data/manager_winner.txt"
manager_winner <- read.csv(url, 
                           header = FALSE, 
                           dec = ",")
colnames(manager_winner) = c('Manager', 'TW', 'ID')
manager_winner = as.data.frame(manager_winner)

## manager of year
url = "~/Google Drive/4893/independent research/data/manager_year.txt"
manager_year <- read.csv(url)
manager_year = as.data.frame(manager_year)

## data merge
df1 = merge(manager, manager_mon, by = 'ID', all.x = TRUE)
df1 = df1[,-6]
df2 = merge(df1, manager_year, by = 'ID', all.x = TRUE)
df2 = df2[,-7]
df3 = merge(df2, manager_winner, by = 'ID', all.x = TRUE)
df3 = df3[, -8]
df3[is.na(df3)] = 0

score = rep(0, nrow(df3))
score = as.vector(score)
for (i in 1:179) {
    score[i] = df3[i, 6] * weight[1] + df3[i, 4] * weight[2]
    + df3[i ,8] * weight[3] + df3[i, 7] * weight[4]
}
df3['SCORE'] = score * 100 ## make it easier to compare.
result = df3[with(df3, order(-SCORE)),]

df4 = subset(df3, MATCHES >= 38)
score1 = rep(0, nrow(df3))
score1 = as.vector(score)
for (i in 1:114) {
    score1[i] = df4[i, 6] * weight[1] + df4[i, 4] * weight[2]
    + df4[i ,8] * weight[3] + df4[i, 7] * weight[4]
}
df3['SCORE'] = score1 * 100
result1 = df4[with(df4, order(-SCORE)),]

df5 = subset(df3, PRESENT == 1)
score2 = rep(0, nrow(df5))
score2 = as.vector(score2)
for (i in 1:20) {
    score2[i] = df5[i, 6] * weight[1] + df5[i, 4] * weight[2]
    + df5[i ,8] * weight[3] + df5[i, 7] * weight[4]
}
df5['SCORE'] = score2 * 100
result2 = df5[with(df5, order(-SCORE)),]

df6 = subset(df3, MATCHES >= 19 & MATCHES <= 114)
score3 = rep(0, nrow(df6))
score3 = as.vector(score3)
for (i in 1:89) {
    score3[i] = df6[i, 6] * weight[1] + df6[i, 4] * weight[2]
    + df6[i ,8] * weight[3] + df6[i, 7] * weight[4]
}
df6['SCORE'] = score3 * 100
result3 = df6[with(df6, order(-SCORE)),]

