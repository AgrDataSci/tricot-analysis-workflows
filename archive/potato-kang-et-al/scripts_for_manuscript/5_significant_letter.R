#overall performance
a1<- summary(mod_o3, ref = ref)
a2<- summary(mod_o3, ref = "Cruza"      )
a3<- summary(mod_o3, ref = "Ndamira")
a4<- summary(mod_o3, ref = "Kazeneza")
a5<- summary(mod_o3, ref = "Izihirwe")
a6<- summary(mod_o3, ref = "Gisubizo")
a7<- summary(mod_o3, ref = "Seka")
a8<-summary(mod_o3, ref = "Twihaze")
a9<-summary(mod_o3, ref = "Jyambere")
a10<- summary(mod_o3, ref = "Ndeze")
a11 <- summary(mod_o3, ref = "Nkunganire")

a1 <- a1$coefficients[,4]
a2 <- a2$coefficients[,4]
a3 <- a3$coefficients[,4]
a4 <- a4$coefficients[,4]
a5 <- a5$coefficients[,4]
a6 <- a6$coefficients[,4]
a7 <- a7$coefficients[,4]
a8 <- a8$coefficients[,4]
a9 <- a9$coefficients[,4]
a10 <- a10$coefficients[,4]
a11 <- a11$coefficients[,4]


alist<-list(a1, a2 ,a3 ,a4, a5, a6, a7,a8,a9,a10, a11)

amatrix<- do.call(rbind, alist)

amatrix[is.na(amatrix)] <- 0  
amatrix
amatrix
colnames(amatrix)
amatrix<- amatrix[,c("Kirundo",
           "Cruza",
           "Ndamira",
           "Kazeneza",
           "Izihirwe",
           "Gisubizo",
           "Seka",
           "Twihaze",
           "Jyambere",
           "Ndeze",
           "Nkunganire")]
amatrix
rownames(amatrix) <- c("Kirundo",
                       "Cruza",
                       "Ndamira",
                       "Kazeneza",
                       "Izihirwe",
                       "Gisubizo",
                       "Seka",
                       "Twihaze",
                       "Jyambere",
                       "Ndeze",
                       "Nkunganire")

amatrix
cxLtrs <- multcompLetters(amatrix, threshold=0.05)
cxLtrs


##
#plant into the future
a1<- summary(mod_f3, ref = ref)
a2<- summary(mod_f3, ref = "Cruza"      )
a3<- summary(mod_f3, ref = "Ndamira")
a4<- summary(mod_f3, ref = "Kazeneza")
a5<- summary(mod_f3, ref = "Izihirwe")
a6<- summary(mod_f3, ref = "Gisubizo")
a7<- summary(mod_f3, ref = "Seka")
a8<-summary(mod_f3, ref = "Twihaze")
a9<-summary(mod_f3, ref = "Jyambere")
a10<- summary(mod_f3, ref = "Ndeze")
a11 <- summary(mod_f3, ref = "Nkunganire")

a1 <- a1$coefficients[,4]
a2 <- a2$coefficients[,4]
a3 <- a3$coefficients[,4]
a4 <- a4$coefficients[,4]
a5 <- a5$coefficients[,4]
a6 <- a6$coefficients[,4]
a7 <- a7$coefficients[,4]
a8 <- a8$coefficients[,4]
a9 <- a9$coefficients[,4]
a10 <- a10$coefficients[,4]
a11 <- a11$coefficients[,4]


alist<-list(a1, a2 ,a3 ,a4, a5, a6, a7,a8,a9,a10, a11)

amatrix<- do.call(rbind, alist)

amatrix[is.na(amatrix)] <- 0  
amatrix
amatrix
colnames(amatrix)
amatrix<- amatrix[,c("Kirundo",
                     "Cruza",
                     "Ndamira",
                     "Kazeneza",
                     "Izihirwe",
                     "Gisubizo",
                     "Seka",
                     "Twihaze",
                     "Jyambere",
                     "Ndeze",
                     "Nkunganire")]
amatrix
rownames(amatrix) <- c("Kirundo",
                       "Cruza",
                       "Ndamira",
                       "Kazeneza",
                       "Izihirwe",
                       "Gisubizo",
                       "Seka",
                       "Twihaze",
                       "Jyambere",
                       "Ndeze",
                       "Nkunganire")

amatrix
cxLtrs <- multcompLetters(amatrix, threshold=0.05)
cxLtrs
##


#overall preference 
a1<- summary(mod_b3, ref = ref)
a2<- summary(mod_b3, ref = "Cruza"      )
a3<- summary(mod_b3, ref = "Ndamira")
a4<- summary(mod_b3, ref = "Kazeneza")
a5<- summary(mod_b3, ref = "Izihirwe")
a6<- summary(mod_b3, ref = "Gisubizo")
a7<- summary(mod_b3, ref = "Seka")
a8<-summary(mod_b3, ref = "Twihaze")
a9<-summary(mod_b3, ref = "Jyambere")
a10<- summary(mod_b3, ref = "Ndeze")
a11 <- summary(mod_b3, ref = "Nkunganire")

a1 <- a1$coefficients[,4]
a2 <- a2$coefficients[,4]
a3 <- a3$coefficients[,4]
a4 <- a4$coefficients[,4]
a5 <- a5$coefficients[,4]
a6 <- a6$coefficients[,4]
a7 <- a7$coefficients[,4]
a8 <- a8$coefficients[,4]
a9 <- a9$coefficients[,4]
a10 <- a10$coefficients[,4]
a11 <- a11$coefficients[,4]


alist<-list(a1, a2 ,a3 ,a4, a5, a6, a7,a8,a9,a10, a11)

amatrix<- do.call(rbind, alist)

amatrix[is.na(amatrix)] <- 0  
amatrix
amatrix
colnames(amatrix)
amatrix<- amatrix[,c("Kirundo",
                     "Cruza",
                     "Ndamira",
                     "Kazeneza",
                     "Izihirwe",
                     "Gisubizo",
                     "Seka",
                     "Twihaze",
                     "Jyambere",
                     "Ndeze",
                     "Nkunganire")]
amatrix
rownames(amatrix) <- c("Kirundo",
                       "Cruza",
                       "Ndamira",
                       "Kazeneza",
                       "Izihirwe",
                       "Gisubizo",
                       "Seka",
                       "Twihaze",
                       "Jyambere",
                       "Ndeze",
                       "Nkunganire")

amatrix
cxLtrs <- multcompLetters(amatrix, threshold=0.05)
cxLtrs

qvcalc(mod_f3, ref = ref)
