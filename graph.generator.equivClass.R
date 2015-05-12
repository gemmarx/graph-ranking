

####
# Parameters
node.num <- 11
edge.num <- 18
randomized.threshold <- 4
iteration.limit <- 7


init.env <- function() {
    combi.initializer <<- init.combi.shuffle
    degree <<- get.degree(node.num, edge.num)
    degree.first <<- degree[1]
    degree.last <<- degree[L(degree)]
    degree.mod <<- degree - degree.last
    equiv.coeff <<- 2^(1:node.num)
}
get.degree <- function(node, edge) dist.mod(2*edge, node)

####
# Routines
L <- length
W <- which
lwc <- function(x) length(which(x))
nn0 <- function(x) ifelse(is.null(x) || is.na(x), 0, x)
ncr <- function(n, r) prod((n:1)[1:r]) / prod(r:1)

dist.mod <- function(n, d) {
    quot <- n %/% d
    rem <- n %% d
    c(rep(1+quot, rem), rep(quot, d-rem))
}

init.combi <- function(n,r) {
    v <- rep(FALSE, n)
    if(1>r) return(v)
    v[1:r] <- TRUE
    rev(v)
}

init.combi.shuffle <- function(n, r) {
    if(1>r) return(rep(FALSE, n))
    v <- c(rep(TRUE,r), rep(FALSE,n-r))
    for(i in 1:r) {
        j <- floor(runif(1, i, 1+n))
        v[c(i,j)] <- v[c(j,i)]
    }
    v
}

init.combi.equal.space <- function(n, r) {
    v <- rep(FALSE, n)
    if(1>r) return(v)
    cell <- dist.mod(n,r)
    cell.offset <- c(0, cumsum(cell))
    for(i in 1:r) {
        d <- floor(runif(1, 1, 1+cell[i]))
        v[d+cell.offset[i]] <- TRUE
    }
    v
}

succ.combi <- function(v) {
    r <- lwc(v)
    h <- W(!v)[1] - 1
    h <- ifelse(is.na(h), 0, h)
    if(r==h) return(-1)
    if(h) {
        p <- h + W(v[-(1:h)])[1]
        v <- succ.combi.1(v,h,p)
    } else p <- W(v)[1]
    v[p-1] <- TRUE
    v[p] <- FALSE
    v
}

succ.combi.1 <- function(v, h, p) {
    n <- p-h-1
    if(1 < n) {
        v[1:(n-1)] <- FALSE
        v[n:(p-2)] <- TRUE
    }
    v
}

build.link <- function(n) {
    lk <- matrix(0,n,n)
    v <- equivClass(lk, 1)$samp(n-1, degree.first)
    lk[1,-1] <- as.integer(v)
    print(lk[1,])
    build.link.1(lk, n-1)
}

build.link.1 <- function(lk, k) {
    w <- node.num
    n <- k-1
    p <- w-n
    r <- degree[p] - sum(lk[,p])
    if(r<0 || n<r) return()
    if(2==k) {
        lk[w-1, w] <- r
        return(check.link(lk))
    }
    build.link.2(lk,k,p,n,r)
}

build.link.2 <- function(lk, k, p, n, r) {
    eq <- equivClass(lk, p)
    if(!eq$cast.able(r)) return()
    if(n < randomized.threshold) return(search.full(lk,k,p,n,r))
    lim <- ceiling(sqrt(ncr(n,r)))
    while(0<lim) {
        lim <- lim - 1
        eq$reset(eq)
        v <- eq$samp(n, r)
        if(-1==v[1]) break
        lk[p,-c(1:p)] <- as.integer(v)
        build.link.1(lk, k-1)
        if(1>r || 1==L(eq$tag)) break
    }
}

search.full <- function(lk, k, p, n, r) {
    v <- init.combi(n,r)
    while(-1!=v[1]) {
        lk[p,-c(1:p)] <- as.integer(v)
        build.link.1(lk, k-1)
        if(1>r) break
        v <- succ.combi(v)
    }
}

check.degree <- function(lk.link) {
    degree.last == sum(lk.link[,node.num])
}

check.reachable <- function(lk.link) {
    0 < min(lk.link + lk.link %*% lk.link)
}

check.link <- function(lk) {
    lk.link <- lk+t(lk)
    if(is.matrix(lk.link) && check.degree(lk.link)
      && check.reachable(lk.link)) {
        print(lk.link)
        q()
    }
}


equivClass <- function(uptri, p) {
    cast.able <- function(r) r <= lwc(as.logical(bond))
    pack.v.in.edge <- function(n, r) c(rep(TRUE, r), rep(FALSE, n-r))

    reset <- function(eq) {
        id <<- eq$id
        tag <<- eq$tag
        room <<- eq$room
        chain <<- eq$chain
        bond <<- eq$bond
        weight <<- eq$weight
        n <<- eq$params$n
    }

    cast.able <- function(r) r <= lwc(as.logical(bond))
    pack.v.in.edge <- function(n, r) c(rep(TRUE, r), rep(FALSE, n-r))

    adjust.v <- function(stay) {
        v <<- rep(FALSE, n)
        for(key in tag) {
            loc <- chain[[key]]
            num <- stay[key]
            if(nn0(num)) v[loc[1:num]] <- TRUE
        }
        v
    }

    d.hondt <- function(n, r) {
        if(0==r) return(rep(FALSE, n))
        m <- L(tag)
        stay <- rep(0, m)
        names(stay) <- tag
        for(i in 1:r) {
            score <- diag(1/(1.4+stay)) %*% room
            x <- which.max(score+rnorm(m, 0, 0.2))
            stay[x] <- 1+ stay[x]
        }
        if(1==m) stay[1] <- 1+r
        adjust.v(stay)
    }

    cast.random <- function(n, r){
        if(0==r) return(rep(FALSE, n))
        if(!cast.able(r)) return(-1)
        m <- L(tag)
        if(1==m) return(pack.v.in.edge(n, r))
        stay <- rep(0, m); names(stay) <- tag
        y <- sample(id, r, prob=weight)
        for(x in y) {stay[x] <- 1+ stay[x]}
        adjust.v(stay)
    }

## init
    w <- ncol(uptri)
    n <- w-p
    n.loc <- (1+p):w
    if(1!=p) p.loc <- 1:(p-1)
    else p.loc <- FALSE
    mat <- matrix(uptri[p.loc, n.loc], ncol=n)
    id.col <- t(equiv.coeff[p.loc]) %*% mat
    id.int <- (id.col + degree.mod[n.loc])

    id <- as.character(id.int)
    tag <- as.character(unique(sort(id.int)))
    chain <- list()
    room <- rep(0, L(tag)); names(room) <- tag
    stay <- NULL
    bond <- degree[n.loc] - apply(mat, 2, sum); names(bond) <- id

    cast <- d.hondt
    samp <- cast.random
    m <- L(tag)
    if(n==m) samp <- cast <- function(n, r) combi.initializer(n,r)
    else if(1==m) cast <- pack.v.in.edge
    else for(i in 1:n) {
        chain[[id[i]]] <- c(chain[[id[i]]], i)
        room[id[i]] <- 1 + nn0(room[id[i]])
    }

    weight <- max(bond)*room[id] + bond
    weight[1>bond] <- 0

    list(
        cast = cast,
        samp = samp,
        adjust = adjust.v,
        reset = reset,
        cast.able = cast.able,
        params = list(p=p, n=n),
        id = id,
        tag = tag,
        chain = chain,
        room = room,
        bond = bond,
        weight = weight,
        matrix = uptri,
        stay = function() stay
    )
}

#init.env()
#m <- rbind(
#c( 0, 1, 0, 1, 1, 1, 0, 0, 0,  0,  0),
#c( 0, 0, 0, 1, 0, 0, 1, 1, 0,  0,  0),
#c( 0, 0, 0, 0, 1, 0, 1, 0, 1,  1,  0)
#)
#lk <- matrix(0,node.num,node.num)
#lk[1:3,] <- m
#print(lk)
#eq <- equivClass(lk, 4)
#eq$samp(7,4)

#q()


####
# Main
init.env()
print(degree)
for(i in 1:iteration.limit) {
    build.link(node.num)
}

