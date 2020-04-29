mdeaths2 = matrix(mdeaths,nrow = 6,ncol = 12)
colnames(mdeaths2) = month.abb
rownames(mdeaths2) = seq(1974,1979)


# VISUALIZATION OF DATASET
bar.color = c('maroon','red','orange','yellow','magenta','pink')
barplot((mdeaths2),ylim = c(0,4000),xlab = 'Month',ylab = 'Number of Deaths',col = bar.color,beside = TRUE,names.arg = month.abb,cex.names = 0.8)
legend("top",legend = 1974:1979,fill = bar.color,horiz = TRUE)

# CALCULATION OF RATE
sumByRow = unname(rowSums(mdeaths2))
rate = vector();
for ( i in 1:(length(sumByRow)-1)) {
  cal = ((sumByRow[i+1] - sumByRow[i])/sumByRow[i]) * 100
  rate = append(rate,cal)
}

colName = vector();
for (i in c(1974:1978))
  colName = append(colName,paste(toString(i),toString((i+1)),sep = "-"))

names(rate) = colName

# VISUALISATON OF RATE
barplot((rate),xlab = 'Rate of deaths per year',ylab = 'Percentage',col = 'red')
legend("top",legend = 'Rate',fill = 'red')
