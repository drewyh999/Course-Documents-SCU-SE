#创建向量
a <- c(1, 2, 5, 3, 6, -2, 4)
b <- c("one", "two", "three")
c <- c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE)

#使用向量
a <- c(1, 2, 5, 3, 6, -2, 4)
a[3]
a[c(1, 3, 5)]
a[2:6] 
a <- c("k", "j", "h", "a", "c", "m")
a[3]
a[c(1, 3, 5)]
a[2:6]

#创建矩阵
y <- matrix(1:20,nrow=5,ncol=4)
y
cells <- c(1,26,24,68)
rnames <- c("R1", "R2")
cnames <- c("C1", "C2") 
mymatrix <- matrix(cells, nrow=2,ncol=2,byrow=TRUE,dimnames=list(rnames,cnames))
mymatrix
#byrow 默认为FALSE，R如何将传入的向量填充入矩阵，byrow时优先填满行
mymatrix <- matrix(cells, nrow = 2,ncol = 2,byrow = FALSE,dimnames = list(rnames, cnames))
mymatrix

#使用矩阵
x <- matrix(1:10, nrow=2)
x
x[2,]
#访问矩阵的方式matrix[row_index,col_index]row_index或col_index缺省则代表访问全部的列或者行
x[,2]
x[1,4]
x[1, c(4,5)]

#创建数组
dim1 <- c("A1","A2")
dim2 <- c("B1","B2","B3")
dim3 <- c("C1","C2","C3","C4")
z <- array(1:24, c(2,3,4),dimnames = list(dim1,dim2,dim3))
z

#创建数据框
patientID <- c(1,2,3,4)
age <- c(25, 34, 28, 52)
diabetes <- c("Type1", "Type2", "Type1", "Type1")
status <- c("Poor", "Improved", "Excellent", "Poor")
patientdata <- data.frame(patientID, age, diabetes, status)
patientdata

#指定数据框中的数据
patientdata[1:2]
patientdata[c("diabetes","status")]
patientdata$age

#使用系数
patientID <- c(1, 2, 3, 4)
age <- c(25, 34, 28, 52)
diabetes <- c("Type1", "Type2", "Type1", "Type1")
status <- c("Poor", "Improved", "Excellent", "Poor")
diabetes <- factor(diabetes)
status <- factor(status, order=TRUE)
patientdata <- data.frame(patientID, age, diabetes, status)
str(patientdata)                               
summary(patientdata)

#创建list
g <- "My First List"
h <- c(25, 26, 18, 39)
j <- matrix(1:10, nrow=5)
k <- c("one", "two", "three")
mylist <- list(title=g, ages=h, j, k)
mylist

