# Copyright 2019 Andreas M. Brandmaier
#  
# Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
#  
# The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

#
# This piece of code reproduces Table 2 from
# Brandmaier, Ghisletta, & von Oertzen (2019). Optimal Planned Missing Data Design for Linear Latent Growth Curve Models
#

designs <- list(c(1,2,3),c(1,2,4), c(1,2,5),
                c(1,3,4),c(1,3,5),c(1,4,5),
                c(2,3,4),c(2,3,5),c(2,4,5),
                c(3,4,5))

intercept.variance = 50
slope.variance = 5
residual.variance = 32
intercept.slope.covariance = .30 * sqrt(intercept.variance * slope.variance)

efferr <- sapply(designs, function(x) {
  
   x<-x-1
  
   sumtisq <- sum(x*x)
   sumti <- sum(x)
   
   eta <- 1/length(x)             
    
   residual.variance/(sumtisq - eta * sumti * 
                              sumti) 
    
    
})

result <- data.frame(
  Design=sapply(designs, function(x){paste0(x,collapse=",")}),
  Vart=round(sapply(designs, function(x){var(x)}),2),
  Efferr=round(efferr,2)
)

print(result)