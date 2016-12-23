shinyServer(function(input, output) {
  
  output$plot1 <- renderPlot({
    n=length(Xrcpp)
    time.Xrcpp=(n-60):n
    time.pred=(n):(n+m-1)
    pre1=predict(m1,n.ahead=m,((n+1):(n+m)))
    tsx=c(1:length(Xrcpp))
    tsxp=c(n:(length(Xrcpp)+length(pre1$pred)))
    tsxpse=c(n+1:length(pre1$se))
    plot(tsx,Xrcpp,xlim=c(500,700),ylim=c(0,25000),xlab='Time',ylab='Observation',type='l',lwd=1.5,col='blue')
    polygon(c(tsxpse,rev(tsxpse)),c(pre1$pred-pre1$se,rev(pre1$pred+pre1$se)),col=rgb(1,0,0,0.25),border=NA)
    polygon(c(tsxpse,rev(tsxpse)),c(pre1$pred+pre1$se,rev(pre1$pred+qnorm(0.975)*pre1$se)),col=rgb(1,0,0,0.125),border=NA)
    polygon(c(tsxpse,rev(tsxpse)),c(pre1$pred-qnorm(0.975)*pre1$se,rev(pre1$pred-pre1$se)),col=rgb(1,0,0,0.125),border=NA)
    lines(tsxp,c(Xrcpp[length(Xrcpp)],pre1$pred),lty=5,lwd=2,col='red')
    lines(tsxpse,pre1$pred+pre1$se,lty=2,lwd=1.5,col='red')
    lines(tsxpse,pre1$pred-pre1$se,lty=2,lwd=1.5,col='red')
    lines(tsxpse,pre1$pred+qnorm(0.975)*pre1$se,lty=3,lwd=1,col='red')
    lines(tsxpse,pre1$pred-qnorm(0.975)*pre1$se,lty=3,lwd=1,col='red')
    title(main="Rcpp Prediction with ARIMA model")
    })
  
  output$plot2<- renderPlot({
    n=length(Xggplot2)
    time.Xggplot2=(n-60):n
    time.pred=(n):(n+m-1)
    pre2=predict(m2,n.ahead=m,((n+1):(n+m)))
    tsx=c(1:length(Xggplot2))
    tsxp=c(n:(length(Xggplot2)+length(pre2$pred)))
    tsxpse=c(n+1:length(pre2$se))
    plot(tsx,Xggplot2,xlim=c(500,700),ylim=c(0,25000),xlab='Time',ylab='Observation',type='l',lwd=1.5,col='blue')
    polygon(c(tsxpse,rev(tsxpse)),c(pre2$pred-pre2$se,rev(pre2$pred+pre2$se)),col=rgb(1,0,0,0.25),border=NA)
    polygon(c(tsxpse,rev(tsxpse)),c(pre2$pred+pre2$se,rev(pre2$pred+qnorm(0.975)*pre2$se)),col=rgb(1,0,0,0.125),border=NA)
    polygon(c(tsxpse,rev(tsxpse)),c(pre2$pred-qnorm(0.975)*pre2$se,rev(pre2$pred-pre2$se)),col=rgb(1,0,0,0.125),border=NA)
    lines(tsxp,c(Xggplot2[length(Xggplot2)],pre2$pred),lty=5,lwd=2,col='red')
    lines(tsxpse,pre2$pred+pre2$se,lty=2,lwd=1.5,col='red')
    lines(tsxpse,pre2$pred-pre2$se,lty=2,lwd=1.5,col='red')
    lines(tsxpse,pre2$pred+qnorm(0.975)*pre2$se,lty=3,lwd=1,col='red')
    lines(tsxpse,pre2$pred-qnorm(0.975)*pre2$se,lty=3,lwd=1,col='red')
    title(main="ggplot2 Prediction with ARIMA model")
  })
  
  output$plot3<-renderPlot({
    n=length(Xdplyr)
    time.Xdplyr=(n-60):n
    time.pred=(n):(n+m-1)
    pre3=predict(m3,n.ahead=m,((n+1):(n+m)))
    tsx=c(1:length(Xdplyr))
    tsxp=c(n:(length(Xdplyr)+length(pre3$pred)))
    tsxpse=c(n+1:length(pre3$se))
    plot(tsx,Xdplyr,xlim=c(500,700),ylim=c(0,25000),xlab='Time',ylab='Observation',type='l',lwd=1.5,col='blue')
    polygon(c(tsxpse,rev(tsxpse)),c(pre3$pred-pre3$se,rev(pre3$pred+pre3$se)),col=rgb(1,0,0,0.25),border=NA)
    polygon(c(tsxpse,rev(tsxpse)),c(pre3$pred+pre3$se,rev(pre3$pred+qnorm(0.975)*pre3$se)),col=rgb(1,0,0,0.125),border=NA)
    polygon(c(tsxpse,rev(tsxpse)),c(pre3$pred-qnorm(0.975)*pre3$se,rev(pre3$pred-pre3$se)),col=rgb(1,0,0,0.125),border=NA)
    lines(tsxp,c(Xdplyr[length(Xdplyr)],pre3$pred),lty=5,lwd=2,col='red')
    lines(tsxpse,pre3$pred+pre3$se,lty=2,lwd=1.5,col='red')
    lines(tsxpse,pre3$pred-pre3$se,lty=2,lwd=1.5,col='red')
    lines(tsxpse,pre3$pred+qnorm(0.975)*pre3$se,lty=3,lwd=1,col='red')
    lines(tsxpse,pre3$pred-qnorm(0.975)*pre3$se,lty=3,lwd=1,col='red')
    title(main="dplyr Prediction with ARIMA model")
  })

  output$plot4<- renderPlot({
    n=length(Xstringr)
    time.Xstringr=(n-60):n
    time.pred=(n):(n+m-1)
    pre4=predict(m4,n.ahead=m,((n+1):(n+m)))
    tsx=c(1:length(Xstringr))
    tsxp=c(n:(length(Xstringr)+length(pre4$pred)))
    tsxpse=c(n+1:length(pre4$se))
    plot(tsx,Xstringr,xlim=c(500,700),ylim=c(0,25000),xlab='Time',ylab='Observation',type='l',lwd=1.5,col='blue')
    polygon(c(tsxpse,rev(tsxpse)),c(pre4$pred-pre4$se,rev(pre4$pred+pre4$se)),col=rgb(1,0,0,0.25),border=NA)
    polygon(c(tsxpse,rev(tsxpse)),c(pre4$pred+pre4$se,rev(pre4$pred+qnorm(0.975)*pre4$se)),col=rgb(1,0,0,0.125),border=NA)
    polygon(c(tsxpse,rev(tsxpse)),c(pre4$pred-qnorm(0.975)*pre4$se,rev(pre4$pred-pre4$se)),col=rgb(1,0,0,0.025),border=NA)
    lines(tsxp,c(Xstringr[length(Xstringr)],pre4$pred),lty=5,lwd=2,col='red')
    lines(tsxpse,pre4$pred+pre4$se,lty=2,lwd=1.5,col='red')
    lines(tsxpse,pre4$pred-pre4$se,lty=2,lwd=1.5,col='red')
    lines(tsxpse,pre4$pred+qnorm(0.975)*pre4$se,lty=3,lwd=1,col='red')
    lines(tsxpse,pre4$pred-qnorm(0.975)*pre4$se,lty=3,lwd=1,col='red')
    title(main="stringr Prediction with ARIMA model")
  })
  
  output$rcpp <- renderValueBox({
    valueBox(
      value = Xrcpp[length(Xrcpp)],
      subtitle = "Recent number of RCPP package download:",
      icon = icon("book"),
      color = "yellow"
    )
  })
  
  output$ggplot <- renderValueBox({
    valueBox(
      value = Xggplot2[length(Xggplot2)],
      subtitle = "Recent number of GGPLOT2 package download:",
      icon = icon("book"),
      color = "blue"
    )
  })
  
  output$dplyr <- renderValueBox({
    valueBox(
      value = Xdplyr[length(Xdplyr)],
      subtitle = "Recent number of DPLYR package download:",
      icon = icon("book"),
      color = "aqua"
    )
  })
  
  output$stringr <- renderValueBox({
    valueBox(
      value = Xstringr[length(Xstringr)],
      subtitle = "Recent number of STRINGR package download:",
      icon = icon("book"),
      color = "light-blue"
    )
  })
  
  output$downloadCsv <- downloadHandler(
    filename = "data.csv",
    content = function(file) {
      write.csv(data, file)
    },
    contentType = "text/csv"
  )
  
  output$rawtable <- renderPrint({
    orig <- options(width = 1000)
    print((tail(data, input$maxrows)),row.names=FALSE)
    options(orig)
  })
})