## A Ranking Estimation System

## In a record matrix to be given,
## each cell shows the record of a game.
## Mark a cell with "u" if the player upside has won.
## Mark a cell with "l" if the player leftside has won.
## Mark a cell with "d" if the game was drawn.


####
## Parameters
max.iteration <- 500
fix.limit <- 10 * .Machine$double.eps

turnout <- 22
point.per.head <- 10

weight.winner <- 3
weight.loser <- 1/3
weight.drawer <- 1

# magic numbers of symbol
radix <- 4
l <- 1
u <- radix - l
d <- radix / 2


####
## Global variables
init.env <- function() {
    record.mother <<- init.record.mother(j2.2014)

    node.num <- turnout
    adjac.mat <- matrix.adjacency22(node.num)
    adjac.mat <- register.ex.edge(adjac.mat, ex.edge)
    match.chart <<- init.match.chart(adjac.mat)

    id <- sample(1:turnout)
    match.shuffle <<- match.chart[id, id]
}


####
## Initial values
team.label <- c(
"コンサドーレ札幌",
"モンテディオ山形",
"水戸ホーリーホック",
"栃木SC",
"ザスパクサツ群馬",
"ジェフユナイテッド千葉",
"東京ヴェルディ",
"横浜FC",
"湘南ベルマーレ",
"松本山雅FC",
"カターレ富山",
"ジュビロ磐田",
"FC岐阜",
"京都サンガF.C.",
"ファジアーノ岡山",
"カマタマーレ讃岐",
"愛媛FC",
"アビスパ福岡",
"ギラヴァンツ北九州",
"V・ファーレン長崎",
"ロアッソ熊本",
"大分トリニータ"
)

j2.2014 <- c(
-99, 1-1, 4-0, 1-1, 1-0, 0-2, 0-0, 0-1, 2-0, 1-0, 2-1, 1-1, 3-2, 0-1, 3-1, 1-1, 0-1, 1-1, 3-0, 2-1, 2-2, 1-1,
2-1, -99, 0-0, 6-1, 1-2, 1-1, 1-2, 2-4, 1-3, 0-0, 1-0, 0-1, 3-1, 1-0, 0-2, 4-0, 2-0, 2-1, 1-2, 2-1, 1-2, 2-0,
0-0, 0-1, -99, 1-2, 2-0, 1-2, 1-1, 2-2, 0-1, 1-2, 2-3, 4-1, 3-2, 5-1, 0-1, 0-0, 0-0, 1-2, 1-1, 0-0, 0-0, 2-1,
2-1, 1-1, 0-1, -99, 3-0, 0-2, 3-2, 1-1, 0-3, 1-2, 2-1, 0-2, 3-0, 2-1, 0-1, 1-2, 3-3, 1-1, 1-1, 1-0, 1-1, 4-0,
3-0, 1-1, 0-1, 2-0, -99, 1-2, 1-0, 0-2, 0-1, 1-3, 2-0, 1-1, 2-2, 0-1, 3-2, 1-0, 3-2, 1-2, 0-0, 2-4, 1-1, 2-1,
2-0, 2-0, 1-0, 0-2, 3-2, -99, 0-0, 0-0, 0-6, 0-1, 2-1, 2-2, 1-0, 3-0, 1-0, 1-1, 1-0, 3-0, 1-3, 1-1, 3-0, 2-1,
0-0, 1-2, 1-0, 1-1, 1-1, 1-1, -99, 1-1, 0-0, 1-3, 0-1, 0-1, 0-1, 1-0, 0-1, 0-1, 1-1, 0-5, 1-0, 1-5, 1-0, 1-1,
2-2, 2-1, 1-1, 3-0, 1-0, 0-0, 0-0, -99, 1-3, 0-2, 2-0, 4-0, 0-1, 0-2, 0-2, 4-2, 0-0, 2-0, 1-0, 1-2, 0-1, 1-1,
2-0, 1-0, 4-2, 2-1, 1-0, 1-1, 1-0, 4-1, -99, 1-1, 2-0, 1-1, 0-0, 3-0, 2-0, 3-1, 3-0, 2-0, 2-0, 1-2, 2-1, 4-0,
1-2, 0-0, 3-0, 2-1, 3-1, 2-1, 1-1, 2-0, 1-4, -99, 2-1, 2-1, 1-0, 2-2, 1-2, 0-0, 2-1, 2-1, 0-1, 0-0, 2-1, 2-0,
0-2, 1-1, 0-3, 1-0, 0-1, 1-1, 0-3, 1-2, 0-1, 3-2, -99, 0-1, 0-0, 1-2, 0-3, 1-1, 1-3, 1-2, 0-2, 1-0, 0-2, 1-1,
0-1, 0-2, 1-0, 2-3, 2-0, 2-0, 1-2, 2-2, 1-2, 1-1, 3-2, -99, 3-1, 2-2, 1-1, 4-2, 2-0, 3-3, 3-1, 1-0, 3-1, 1-1,
1-1, 1-0, 0-2, 1-3, 1-0, 2-2, 3-0, 1-2, 2-3, 3-1, 3-0, 0-4, -99, 2-1, 2-2, 3-1, 4-3, 1-2, 1-1, 1-1, 2-3, 2-3,
1-1, 2-2, 1-1, 0-0, 0-3, 3-3, 1-0, 2-1, 2-2, 0-0, 1-1, 2-3, 0-0, -99, 1-1, 4-1, 0-0, 3-1, 1-1, 2-0, 0-0, 2-2,
2-0, 1-4, 1-1, 3-1, 1-2, 1-0, 2-1, 0-0, 0-0, 0-0, 0-0, 1-1, 2-1, 2-3, -99, 2-2, 1-1, 1-1, 0-3, 2-1, 1-1, 1-1,
1-0, 0-3, 0-0, 0-1, 1-0, 0-1, 0-1, 0-1, 0-2, 0-5, 2-1, 1-4, 1-2, 2-2, 2-1, -99, 1-2, 1-1, 1-1, 0-1, 1-1, 0-1,
2-3, 4-0, 0-2, 0-1, 0-2, 2-2, 2-1, 2-1, 1-0, 1-4, 4-0, 0-1, 0-0, 0-0, 2-3, 2-0, -99, 0-0, 2-1, 0-3, 4-0, 1-2,
2-2, 0-1, 0-1, 2-0, 1-1, 1-0, 0-1, 1-0, 0-0, 1-2, 2-1, 3-1, 1-0, 1-0, 2-3, 1-2, 1-1, -99, 0-1, 2-5, 1-3, 1-2,
2-0, 0-1, 1-0, 0-1, 2-1, 1-0, 2-1, 2-1, 0-4, 0-0, 2-2, 3-2, 2-0, 1-3, 2-1, 2-1, 0-3, 3-5, -99, 2-1, 1-1, 1-1,
0-1, 0-0, 1-1, 1-1, 2-0, 2-1, 0-0, 1-0, 0-3, 0-2, 2-0, 1-1, 0-2, 0-1, 1-1, 1-1, 2-1, 0-0, 1-1, -99, 0-1, 0-0,
0-2, 1-3, 2-1, 2-1, 0-1, 0-0, 0-0, 2-2, 1-3, 0-1, 2-0, 0-0, 0-3, 1-4, 0-0, 4-1, 3-1, 2-1, 0-1, 1-1, -99, 1-1,
1-0, 1-0, 2-3, 2-1, 2-1, 2-4, 3-2, 1-0, 2-3, 0-2, 3-0, 2-0, 1-0, 0-3, 1-0, 1-0, 2-2, 3-0, 1-0, 1-1, 0-1,  -99
)

matrix.adjacency22 <- function(node) {
    mat <- matrix(0,node,node)
mat[1,c(3,4,5,6,7)] <- 1
mat[2,c(3,4,5,6,7)] <- 1
mat[3,c(1,2,8,9,10)] <- 1
mat[4,c(1,2,11,12,13)] <- 1
mat[5,c(1,2,14,15,16)] <- 1
mat[6,c(1,2,17,18,19)] <- 1
mat[7,c(1,2,20,21,22)] <- 1
mat[8,c(3,11,14,17,20)] <- 1
mat[9,c(3,12,15,18,21)] <- 1
mat[10,c(3,13,16,19,22)] <- 1
mat[11,c(4,8,15,17,22)] <- 1
mat[12,c(4,9,16,18,20)] <- 1
mat[13,c(4,10,14,19,21)] <- 1
mat[14,c(5,8,18,20,13)] <- 1
mat[15,c(5,9,19,21,11)] <- 1
mat[16,c(5,10,17,22,12)] <- 1
mat[17,c(6,8,21,11,16)] <- 1
mat[18,c(6,9,22,12,14)] <- 1
mat[19,c(6,10,20,13,15)] <- 1
mat[20,c(7,8,12,14,19)] <- 1
mat[21,c(7,9,13,15,17)] <- 1
mat[22,c(7,10,11,16,18)] <- 1
    mat
}

ex.edge <- t(matrix(c(
    1,8,
    2,11,
    3,14,
    4,17,
    5,20,
    6,9,
    7,12,
    10,15,
    13,18,
    16,21,
    19,22
), nrow=2))


####
## Routines
# for the global environment
init.record.mother <- function(record) {
    t(matrix(sapply(record, function(x){
        if(-99==x) return(0)
        if(0==x) return(d)
        if(0<x) return(l)
        if(0>x) return(u)
    }), turnout))
}

register.ex.edge <- function(mat, edges) {
    apply(edges, 1, function(y){
        i <- y[1]; j <- y[2]
        mat[i,j] <<- mat[j,i] <<- 1
    })

    mat
}

init.match.chart <- function(adjac) {
    host.game <- function(host) {
        for(i in 1:turnout) {
            if(!adjac[host,i] || match.chart[host,i]) next
            y <- match.chart[host,]
            home <- length(which(1==y))
            if(2<home || propose.game(host, i)) break
        }
    }

    propose.game <- function(host, guest) {
        y <- match.chart[guest,]
        away <- length(which(-1==y))
        if(2<away) return(FALSE)
        match.chart[host, guest] <<- 1
        match.chart[guest, host] <<- -1
        host.game(guest)

        TRUE
    }

    match.chart <<- matrix(0,turnout,turnout)
    for(i in 1:turnout) host.game(i)
    
    match.chart
}

# for the main
take.match.record <- function(record.base, matches) {
    record <- matrix(0, turnout, turnout)
    for(i in 1:turnout) { for(j in 1:turnout) {
        if(1==matches[i,j]) record[i,j] <- record.base[i,j]
        if(-1==matches[i,j]) record[i,j] <- radix - record.base[j,i]
    }}

    record
}

make.dist.ratio <- function(record) {
    diag(record) <- d
    w <- matrix(0,turnout,turnout)
    w[record==l] <- weight.winner
    w[record==u] <- weight.loser
    w[record==d] <- weight.drawer

    apply(w,2, function(x){x/sum(x)})
}

get.score <- function(x){apply(x,1,sum)}

update.score <- function(ratio, dist.prev, will.show=TRUE) {
    score <- get.score(dist.prev)
    if(will.show==TRUE) pp(score)

    ratio %*% diag(score)
}

pp <- function(x,r=2){print(round(x,r))}


####
## Main
init.env()

# calculation of scores
record <- take.match.record(record.mother, match.shuffle)
y <- dist.ratio <- make.dist.ratio(record)
iter <- 0; err <- NULL
for (iter in 1:max.iteration) {
    y.prev <- y
    y <- update.score(dist.ratio, y, FALSE)
    err <- max(abs(y - y.prev))
    if(err<fix.limit) break
}

# output format
score <- point.per.head*get.score(y)
names(score) <- team.label

fact <-apply(record, 1, function(y){
    table(factor(y, levels=0:3), exclude=0)
})
s.f <- cbind(score, t(fact))
s.f <- s.f[rev(order(s.f[,1])),]

pvalue <- rep(0, turnout)
for(i in 2:turnout){
    val <- fisher.test(s.f[c(i-1,i), -1])
    vp <- round(100*val$p.value, 0)
    pvalue[i] <- ifelse(100==vp, 99, vp)
}
s.f.p <- cbind(s.f, pvalue)

# ranking
colnames(s.f.p) <- c("評価点", "勝", "分", "負", "p[%]")
pp(s.f.p)

# conditions
cat(sprintf("iteration: %d    error: %.2e\n", iter, err))

# match details
suffix <- match.out <- matrix("", turnout,turnout)
suffix[-1==match.shuffle] <- "*"
match.out[l==record] <- "o"
match.out[u==record] <- "x"
match.out[d==record] <- "="
match.out <- matrix(paste(match.out, suffix, sep=""), turnout)
rownames(match.out) <- colnames(match.out) <- team.label
print(t(match.out))

