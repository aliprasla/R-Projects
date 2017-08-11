#question one
num_col = 20
num_rows = 20
a = c("Na")
a = matrix(a, num_col,num_rows)
 for (i in 1:num_col){
   for (j in 1:num_rows) {
     if (j >= i)
     {
       a[i,j] = as.numeric(i / j)

     }
     else {
       a[i,j] = as.numeric(j / i)

     }
   }
 }
#convert matrix a to numerical values
dims = dim(a)
a = as.numeric(a)
dim(a) = dims
#question two
print(all.equal(a,t(a)))
#question three
#calc identity matrix
ident = diag(1,num_rows,num_col)
#find inverse
c = a %*% ident
#c is the inverse of a
#confirm?
print(all.equal(a %*% c,c%*%a))
#confirmed.
#question 4
a = matrix(c(3,1,3,-1,0,-2,2,3,-5),3,3)
print(a)
b = matrix(c(3,7,-1,-6,-14,2,-3,-7,1),3,3)
print(b)
#rconfirms ab = 0. ba != ab
#problem 5
a = matrix(c(1,.45,.25,-.1,1,-.55,-.75,.05,1,0,.25,.05,1,0,.25,-.05),4,4)
print(a)
c = matrix(c(250000000,0,0,0),4,1)
inv = solve(a)
print(inv %*% c)
