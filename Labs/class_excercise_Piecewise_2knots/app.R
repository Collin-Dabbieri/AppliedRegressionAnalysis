# Load libraries needed
library(shiny)
#library(ggplot2)
library(purrr)
library(rootSolve)
source("Rcode.R")
library(rgl)
library(plotly)

spruce.df = read.csv("SPRUCE.csv")

d = spruce.df$BHDiameter



# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Spruce Data Set: Piecewise Regression"),
   
   # Sidebar with a slider input for number of bins 
   
   sidebarLayout(
      sidebarPanel(
         sliderInput("xk1",
                     "Choose knot number 1:",
                     min = min(d),
                     max = max(d),
                     value = 17.44165,
                     step=0.01),
         sliderInput("xk2",
                     "Choose knot number 2:",
                     min = min(d),
                     max=max(d),
                     value=20.0,
                     step=0.01),
         
         sliderInput("n",
                     "Choose sampling for xk1 and xk2:",
                     min=10,
                     max=100,
                     value=50,
                     step=1)
         
         
                     
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("regressPlot"),
         plotlyOutput("R2_contour"),
         rglwidgetOutput("R2_3D"),
         #tableOutput("root"),
         # table of data
         tableOutput("tab")
         #plotOutput("allroots")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   output$tab <- renderTable(spruce.df)
   

   
   output$regressPlot <- renderPlot({
     plot(spruce.df,main="Piecewise regression",pch=21,bg="black")
     
     
     #sp2.df=within(spruce.df, X1<-(BHDiameter-input$xk1)*(BHDiameter>input$xk1),X2<-(BHDiameter-input$xk2)*(BHDiameter>input$xk2))  
      #lmp = lm(Height ~ BHDiameter + X1+X2, data = sp2.df)
      #tmp=summary(lmp) # tmp holds the summary info
      #print(tmp$coefficients[,"Estimate"])
     B2=with(spruce.df,(BHDiameter-input$xk1)*(BHDiameter>input$xk1))
     B3=with(spruce.df,(BHDiameter-input$xk2)*(BHDiameter>input$xk2))
     lmp=lm(Height ~ BHDiameter + B2 + B3,data=spruce.df)
     tmp=summary(lmp)
     print(tmp)
     #print(tmp$coefficients[,"Estimate"])
      
   
      
      
      
      curve(myf(x,xk1=input$xk1,xk2=input$xk2,coef=tmp$coefficients[,"Estimate"] ),
            add=TRUE, 
            lwd=2,
            col="Blue")
      
     points(input$xk1,myf(input$xk1,input$xk1,input$xk2,coef=tmp$coefficients[,"Estimate"] ),col="black",pch=21,bg="green",cex=2)
     points(input$xk2,myf(input$xk2,input$xk1,input$xk2,coef=tmp$coefficients[,"Estimate"] ),col="black",pch=21,bg="green",cex=2)
     
     #points(max_loc[1],myf(max_loc[1],max_loc[1],max_loc[2],coef=tmp$coefficients[,"Estimate"]),col="purple",pch=21,bg="green",cex=2)
     
     #points(uroot()$root,myf(uroot()$root,uroot()$root,coef=tmp$coefficients[,"Estimate"] ),col="black",pch=21,bg="purple",cex=2) 
     
      text(20,16,
           paste("R sq.=",round(tmp$r.squared,4) ))
      
   }) 
   

   
   
   output$R2_contour <- renderPlotly({
      
      #We're defining the grid search for R^2 values outside of the function so we only have to do it once
      xk1_range = seq(min(d),max(d),length=input$n)
      xk2_range = seq(min(d),max(d),length=input$n)
      R2=matrix(, nrow = length(xk1_range), ncol = length(xk2_range))
      
      max_R2=0
      
      #fill R2 matrix for contour and find highest R^2 value
      for (i in 1:length(xk1_range)){
         for (j in 1:length(xk2_range)){
            if (j>i){
               xk1_val=xk1_range[i]
               xk2_val=xk2_range[j]
               R2[j,i]=rsq2(xk1_val,xk2_val,spruce.df)
               if (R2[j,i]>max_R2){
                  max_R2<-R2[j,i]
                  max_loc=c(xk1_val,xk2_val)
               }
               
            }
         }
      }
      
      f <- list(
         family = "Courier New, monospace",
         size = 18,
         color = "#7f7f7f"
      )
      x <- list(
         title = "xk1",
         titlefont = f
      )
      y <- list(
         title = "xk2",
         titlefont = f
      )
      plot_ly(x=xk1_range,
              y=xk2_range,
              z=R2,
              type="contour"
              )%>%
         add_trace(x=max_loc[1],y=max_loc[2],type="scatter",color='red')%>%
         layout(title="Contour Plot for R^2",xaxis = x, yaxis = y)

     
     
   })
   
   output$R2_3D <- renderRglwidget({
      xk1_range = seq(min(d),max(d),length=input$n)
      xk2_range = seq(min(d),max(d),length=input$n)
      
      R2=matrix(, nrow = length(xk1_range), ncol = length(xk2_range))
      
      max_R2=0
      
      #fill R2 matrix for contour and find highest R^2 value
      for (i in 1:length(xk1_range)){
         for (j in 1:length(xk2_range)){
            if (j>i){
               xk1_val=xk1_range[i]
               xk2_val=xk2_range[j]
               R2[j,i]=rsq2(xk1_val,xk2_val,spruce.df)
               if (R2[j,i]>max_R2){
                  max_R2<-R2[j,i]
                  max_loc=c(xk1_val,xk2_val)
               }
               
            }
         }
      }
      
      #create x,y,z coords for rgl plot
      x=c()
      y=c()
      z=c()
      for (i in 1:length(xk1_range)){
         for (j in 1:length(xk2_range)){
            if (j>i){
               x=append(x,xk1_range[i])
               y=append(y,xk2_range[j])
               z=append(z,R2[j,i])
            }
         }
      }
      
      try(rgl.close())
      z_stand=(z-mean(z))/sd(z)
      z_stand=z_stand+abs(min(z_stand))
      z_stand=z_stand/max(z_stand)
      plot3d(x, y, z,xlab="xk1",ylab="xk2",zlab="R^2",col=rgb(z_stand,1-z_stand,0))
      rglwidget()
   })
   
 
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

