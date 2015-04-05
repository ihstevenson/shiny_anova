library(shiny)

# Define server logic for random distribution application
shinyServer(function(input, output, clientData, session) {
  
  # Reactive expression to generate the requested distribution.
  # This is called whenever the inputs change. The output
  # functions defined below then all use the value computed from
  # this expression
  get.datalist <- reactive({
    ttt<-c(input$text1,input$text2,input$text3,input$text4,input$text5,
           input$text6,input$text7,input$text8)
    
    value<-NULL
    condition<-NULL
    df<-data.frame(condition, value);
    for (i in 1:length(ttt)) {
      #value = as.numeric(read.csv(textConnection(ttt[i]),sep=" ",header=F))
      value = NA
      tryCatch(  { value = as.numeric(read.csv(textConnection(ttt[i]),sep=" ",header=F)) }, error = function(e) {}  )
      condition = as.character(i)
      if (input$atype=="indep") {
        df <- rbind(df,data.frame(condition, value))
      } else {
        subject = as.character(1:length(value))
        df <- rbind(df,data.frame(condition, value, subject))
      }
        
    }
    na.omit(df)
    
  })
  
  observeEvent(input$clr, {
    print("clearing...")
    updateTextInput(session, "text1",value = "")
    updateTextInput(session, "text2",value = "")
    updateTextInput(session, "text3",value = "")
    updateTextInput(session, "text4",value = "")
    updateTextInput(session, "text5",value = "")
    updateTextInput(session, "text6",value = "")
    updateTextInput(session, "text7",value = "")
    updateTextInput(session, "text8",value = "")
  })
  
  get.datamat <- function(df) {
    st <- matrix(,nrow=dim(df)[1],ncol=max(as.numeric(df$condition)))
    count<-1:max(as.numeric(df$condition));
    count<-count*0+1
    for (i in 1:dim(df)[1]) {
      st[count[as.numeric(df[i,1])],as.numeric(df[i,1])] <- as.numeric(df[i,2])
      count[as.numeric(df[i,1])] = count[as.numeric(df[i,1])]+1;
      #print(st)
    }
    st<-st[1:(max(count)-1),]
    st<-data.frame(st)

    grand_mean <- mean(as.matrix(st),na.rm=T)
    st<-rbind(st,colMeans(st,na.rm = T))
    st<-rbind(st,mapply(var,st,na.rm=T)*(colSums(is.finite(as.matrix(st)))-1))
    colnames(st) <- lapply(1:dim(st)[2], function(i) {paste0('Condition ',i)})
    if (input$atype=="repm") {
      row.names(st) <- lapply(1:dim(st)[1], function(i) {paste0('Subject ',i)})
      st<- cbind(st,format(rowMeans(st,na.rm=T),digits=3))
      st[dim(st)[1]-1,dim(st)[2]]<-paste0('M_all=',format(grand_mean,digits=3))
      st[dim(st)[1],dim(st)[2]]<-NA
    } else {
      st[dim(st)[1]-1,dim(st)[2]+1]<-paste0('M_all=',format(grand_mean,digits=3))
    }
    colnames(st)[dim(st)[2]]<-"Means"
    row.names(st)[dim(st)[1]-1]<-"Means"
    row.names(st)[dim(st)[1]]<-"SS"
    
    return(st)
  }
  
  
  # Generate a plot of the data. Also uses the inputs to build
  # the plot label. Note that the dependencies on both the inputs
  # and the data reactive expression are both tracked, and
  # all expressions are called in the sequence implied by the
  # dependency graph
  output$plot <- renderPlot({
    my.data <- get.datalist()
    
    x <- 1:length(unique(my.data$condition))
    avg <- aggregate(my.data,by=list(my.data$condition),FUN=mean)[[3]];
    sdev <- aggregate(my.data,by=list(my.data$condition),FUN=sd)[[3]];
    sdev[!is.finite(sdev)] <- 0;
    
    #print(x)
    #print(avg)
    #print(sdev)
    
    plot(x, avg,
         xlim=range(as.numeric(my.data$condition))+c(-0.5,0.5),ylim=range(c(my.data$value,avg+sdev,avg-sdev)),
         pch=19, xlab="Conditions", ylab="Mean +/- SD",frame=F,xaxt="n")
    axis(1, at=1:max(as.numeric(my.data$condition)))    
    points(as.numeric(my.data$condition),my.data$value)
    arrows(x, avg-sdev, x, avg+sdev, length=0.05, angle=90, code=3)
  })
  
  output$Fplot <- renderPlot({
    tcol="orange"      # fill colors
    acol="orangered"   # color for added samples
    tscale=1;          # label rescaling factor
    
    x0 = seq(0,10,length.out=512);
    my.data<-get.datalist()
    if (input$atype=="indep") {
      df1<-max(as.numeric(my.data$condition))-1
      df2<-dim(my.data)[1]-(df1+1)
    } else {
      df1<-max(as.numeric(my.data$condition))-1
      df2<-(max(as.numeric(my.data$subject))-1)*(max(as.numeric(my.data$condition))-1)
    }
    y0 = df(x0, df1, df2)
    y0=y0/sum(y0[2:length(y0)]);
    plot(x0,y0,type="l",lwd=2,col="black",main="F Distribution",xlab="F(df1,df2)",ylab="",frame=F,cex.lab=tscale, cex.axis=tscale, cex.main=tscale, cex.sub=tscale,yaxt="n")
    
    xcr<-x0[x0>qf(1-input$alpha,df1,df2)];
    polygon(c(xcr[1],xcr,10),c(0,y0[x0>qf(1-input$alpha,df1,df2)],0),col=tcol)
    

    if (input$atype == "indep") {
      aov.ex2 = aov(value~condition,data=get.datalist())
      ft<-summary(aov.ex2)[[1]][["F value"]][1]
    } else {
      aov.ex2 = aov(value~condition+Error(subject/condition),get.datalist())
      ft<-summary(aov.ex2)[[2]][[1]][["F value"]][1]
    }
    lines(c(ft,ft), c(min(y0),max(y0)),col=acol,lwd=2)
    
  })
  
  # Generate a summary of the data
  output$summary <- renderPrint({
    print(get.datalist())
  })
  
  # Generate a summary of the data
  output$summary_mat <- renderTable({
    get.datamat(get.datalist())
  })
  
  # Generate an HTML table view of the data
  output$table <- renderTable({
    if (input$atype == "indep") {
      aov.ex2 = aov(value~condition,data=get.datalist())
    } else {
      aov.ex2 = aov(value~condition+Error(subject/condition),get.datalist())
    }
    tout<-xtable(summary(aov.ex2),digits=2)
    if (input$atype=="indep") {
      tout <- rbind(tout,c(sum(tout[,1]),sum(tout[,2]),sum(tout[,2])/sum(tout[,1]),NA,NA))
      rownames(tout)<-c("Betwen Groups","Within Groups","Total")
    } else {
      if (dim(tout)[1]==3) {
        tout <- tout[c(2,1,3),]
        tout <- rbind(tout[1,],c(sum(tout[c(2,3),1]),sum(tout[c(2,3),2]),sum(tout[c(2,3),2])/sum(tout[c(2,3),1]),NA,NA),tout[c(2,3),])
        tout <- rbind(tout,c(sum(tout[c(1,2),1]),sum(tout[c(1,2),2]),sum(tout[c(1,2),2])/sum(tout[c(1,2),1]),NA,NA))
        rownames(tout)<-c("Betwen Groups","Within Groups","Between Subjects","Error","Total")
      }
    }
    tout
  })
  
})