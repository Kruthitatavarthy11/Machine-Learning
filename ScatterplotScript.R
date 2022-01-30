mtcars
x <- mtcars$wt
y <- mtcars$mpg
# Plot with main and axis titles
# Change point shape (pch = 19) and remove frame.
plot(x, y, main = "Main title",
     xlab = "X axis title", ylab = "Y axis title",
     pch = 19, frame = FALSE)
# Add regression line
plot(x, y, main = "Main title",
     xlab = "X axis title", ylab = "Y axis title",
     pch = 19, frame = FALSE)
abline(lm(y ~ x, data = mtcars), col = "blue")

# Add fit
plot(x, y, main = "Main title",
     xlab = "X axis title", ylab = "Y axis title",
     pch = 19, frame = FALSE)
lines(lowess(x, y), col = "blue")


install.packages("car")

library("car")
#scatterplot using car library
scatterplot(wt ~ mpg, data = mtcars)

#Scatterplot by groups
scatterplot(wt ~ mpg | cyl, data = mtcars, 
            smoother = FALSE, grid = FALSE, frame = FALSE)


