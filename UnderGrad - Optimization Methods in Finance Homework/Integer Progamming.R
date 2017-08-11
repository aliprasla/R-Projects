library ('lpSolve')
#question 1 - inital
a = matrix(c(-10,5,1,20,10,0),3,2)
b = matrix(c(22,49,5),3,1)
c = c(-1,4)
dir = rep("<=",3)
sol = lp('max',c,a,dir,b)
print(sol$solution)
print(sol$objval)
# 1 = 3.8 | 2 = 3.0 - 8.2
#add constraint x1 >= 4
a = matrix(c(-10,5,1,-1,20,10,0,0),4,2)
dir = rep('<=',4)
b = c(22,49,5,-4)
sol = lp('max',c,a,dir,b)
print(sol$solution)
#sol = 4.0 2.9 - 7.6
print(sol$objval)


#add constraint x1 <= 3
a = matrix(c(-10,5,1,1,20,10,0,0),4,2)
b = c(22,49,5,3)
dir = rep('<=',4)
sol = lp('max',c,a,dir,b)
print(sol$solution)
print(sol$objval)
#1 = 3.0 2 = 3.6
#add constraint x2 >= 4
a = matrix(c(-10,5,1,1,0,20,10,0,0,-1),5,2)
print(a)
b = c(22,49,5,3,-4)
dir = rep('<=',5)
sol = lp('max',c,a,dir,b)
print(sol$solution)
#1 = 3.0 2 = 3.6

#add constraint x2 <= 3

a = matrix(c(-10,5,1,1,0,20,10,0,0,1),5,2)
b = c(22,49,5,3,3)
dir = rep('<=',5)
sol = lp('max',c,a,dir,b)
print(sol$solution)
print(sol$objval)

a = matrix(c(-10,5,1,1,0,0,20,10,0,0,1,1),6,2)
b = c(22,49,5,3,3,2)
dir = rep('<=',6)
sol = lp('max',c,a,dir,b)
print(sol$solution)
print(sol$objval)


#sol 3 , 2.6


#comeback to branch one.. x1 = 4.0 x2 = 2.9
a = matrix(c(-10,5,1,-1,0,20,10,0,0,1),5,2)
print(a)
b = c(22,49,5,-4,2)
print(b)
dir = rep('<=',5)
sol = lp('max',c,a,dir,b)
print(sol$objval)
print(sol$solution)
#move right - x2 >= 3
a = matrix(c(-10,5,1,-1,0,20,10,0,0,-1),5,2)
b = c(22,49,5,-4,-3)
dir = rep('<=',5)
sol = lp('max',c,a,dir,b)
print(sol$objval)
print(sol$solution)
#solution is zero, zero, with zero.
a = matrix(c(-10,5,1,1,20,10,0,0),4,2)
b = c(22,49,5,3)
dir = rep('<=',4)
sol = lp('max',c,a,dir,b)
print(sol$solution)
print(sol$objval)

#confirm with r
a = matrix(c(-10,5,1,20,10,0),3,2)
b = matrix(c(22,49,5),3,1)
c = c(-1,4)
dir = rep("<=",3)
sol = lp('max',c,a,dir,b,all.int= TRUE)
print(sol$solution)
-
print(sol$objval)


#-----------------Problem 2----------------------
a = matrix(c(6,-1,0,5,0,1,3,-1,0,2,0,1),3,4)
b = c(11,-1,1)
c = c(3,1,2,2)
dir = rep("<=",3)
sol = lp('max',c,a,dir,b,all.int = TRUE,all.bin = TRUE)
print(sol$solution)
print(sol$objval)
#solution 1 0 1 1 -------- 7 million dollars

#----------------Problem 3 ----------------------
#populate matrix a
a = matrix(0,14,14)
a[1:7,1:7] = diag(7)
a[8:14,1:7] = diag(7)
a[9:14,8:13] = diag(6)
a[1,8] = 1
a[2:7,8:13] = diag(6)
a[6,1] = -1
a[7,2] = -1
a[8,1] = 1
a[13,1] = -1
a[14,2] = -1
#a[2:7,9:14] = diag(x=-1,6,6)
for (i in 0:5) {
  a[2+i,9+i] = -1
}
dir = c(rep('=',7),rep('>=',7))
b = c(rep(0,7),5,13,12,10,14,8,6)
c = c(90,rep(60,5),90)
sol = lp('min',c,a,dir,b,all.int= TRUE)
print(sol$solution)