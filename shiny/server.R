library(shiny)
library(DT)

ep2=read.csv("got_predictions_EP2_fixed.csv")


not_dead_predictions=read.csv("got_predictions_EP3_fixed.csv")

ge=merge(not_dead_predictions,ep2[,c(1,3)],by="name")
ge$dead_rate_change=ge$dead_prob.x-ge$dead_prob.y


not_dead_predictions$image=paste0('<img src="got_images/',not_dead_predictions$image,'" height="80"></img>')
not_dead_predictions=not_dead_predictions[,-2]

not_dead_predictions$highprob=as.numeric(as.character(not_dead_predictions$dead_prob))>.5
not_dead_predictions$highprob=as.numeric(not_dead_predictions$highprob)

not_dead_predictions=merge(not_dead_predictions,ge[,c(1,9)],by='name')
not_dead_predictions=not_dead_predictions[,c(1:5,8,6,7)]
server = function(input, output) {
  ndp=datatable(not_dead_predictions,options = list(order=list(1,"desc"),columnDefs = list(list(className = 'dt-center', targets = 1),list(targets = 7, visible = FALSE)),
                                                    pageLength = 20, autoWidth = TRUE), filter = 'top',class = 'hover',
                escape = FALSE,rownames=F,colnames = c('Name', 'Probability of Death', 'Current Location', 'Main House Interaction', 'House Allegiance',"Change from Previous EP","Character Image","High Prob")) %>% formatPercentage('dead_prob', 2) %>% formatPercentage('dead_rate_change', 2) %>% formatStyle(
                  "highprob",
                  target = 'row',
                  backgroundColor = styleEqual(c(0, 1), c('#B2DDB2', '#ddb2b2'))
                )%>% formatStyle("name",fontWeight="bold")%>% formatStyle("dead_rate_change", color = styleInterval(c(0, .00000000000001), c('red', 'grey', 'green')),fontWeight="bold") %>%  formatStyle("dead_prob",fontWeight="bold")
  output$tbl = DT::renderDataTable(
    ndp
  )
}