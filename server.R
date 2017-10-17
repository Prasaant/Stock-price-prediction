
library(shiny)
comp<-read.csv("company.csv")

mypredict<-function(st){
  lm.st<-lm(formula = price~dateno,data = st)
  
  predict.lm(lm.st,data.frame(dateno=(((tail(st,1))[,1])+1)))
}
doublepredict<-function(st){
  price<-st[,3]
  
  no<-st[,1]
  ReturnOnAssets <- (round((st[,4]/st[,5])*100))
  ProfitMargin <- (round((st[,4]/st[,6])*100))
 
  l<-lm(price~ no+ReturnOnAssets+ProfitMargin)

  predict.lm(l,data.frame((no=(((tail(st,1))[,1])+1)),ReturnOnAssets=0,ProfitMargin=0))
 
  }
singlepredict<-function(st,sing){
  if(sing=="2"){
    price<-st[,3]
    no<-st[,1]
    ReturnOnAssets <- (round((st[,4]/st[,5])*100))
    
    l<-lm(price~ no+ReturnOnAssets)
    predict.lm(l,data.frame((no=(((tail(st,1))[,1])+1)),ReturnOnAssets=0))
  }
  else if(sing=="3"){
    price<-st[,3]
    no<-st[,1]
    ProfitMargin <- (round((st[,4]/st[,6])*100))
    
    l<-lm(price~ no+ProfitMargin)
    predict.lm(l,data.frame((no=(((tail(st,1))[,1])+1)),ProfitMargin=0))
  }
}

shinyServer(function(input, output) {
  output$data_table <- renderDataTable({
    X <- subset(comp, company==input$sin, select = c(company, link))
    st<-read.csv(toString(X[,2]))
    data.frame(st)
  })
  
  output$graph<-renderPlot({
    X <- subset(comp, company==input$sin, select = c(company, link))
    st<-read.csv(toString(X[,2]))
    lm.st<-lm(formula = price~dateno,data = st)
    plot(price~date,data=st,xlab="date",ylab="price")
    
    abline(lm.st)
    })
  
  output$distPlot <- renderText( {
    X <- subset(comp, company==input$sin, select = c(company, link))
    
    st<-read.csv(toString(X[,2]))
    if(length(input$predictors)==0){
      mypredict(st)
      
    }
    else {
      roavar<-input$predictors[1]
      profitmarginvar<-input$predictors[2]
      
      
      if(toString(roavar)!="NA"&&toString(profitmarginvar)!="NA"){
        doublepredict(st)
      }
      else if(toString(roavar)!="NA"){
        
        singlepredict(st,roavar)
      }
      else if(toString(profitmarginvar)!="NA"){
        singlepredict(st,profitmarginvar)
      }
      
    }
    })

})
