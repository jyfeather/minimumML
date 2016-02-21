data("iris")                          # load data

X <- t(log(iris[,1:4]))               # log transform
m <- nrow(X)
n <- ncol(X)
mean <- rowMeans(X) 
X <- X - replicate(n, mean)           # subtract mean

Cx <- (1/(n-1))*X %*% t(X)            # covariance matrix
res.eig <- eigen(Cx)                  # eigenvalue decomposition
lambda <- res.eig$values              # eigenvalues as variance explained
V <- res.eig$vectors                  # eigenvectors as loadings

Y <- t(V) %*% X                       # produce the principal components projection

Cy <- (1/(n-1))*Y%*%t(Y)              # check Cy and Cx

plot(1:m, lambda, xlab = "# of PCA")