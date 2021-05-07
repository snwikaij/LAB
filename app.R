library(readxl)
library(openxlsx)

####################################################################################################
ui <- fluidPage(
    titlePanel("My name is ICP-MS from the University Duisburg-Essen."),
    
                             sidebarLayout(
                                 sidebarPanel(
                                              fileInput('file1', 'Choose Excel file',
                                                        multiple = FALSE,
                                                        accept = c(".xlsx")),
                                              h4(""),
                                              textOutput("warn"),
                                              h3(""),
                                              downloadButton("downloadData", "Download converted files"),
                                              h3(""),
                                              downloadButton("downloadExample", "Download example file for uploading")),
                                 mainPanel(DT::dataTableOutput("mytable", width="50%"))))


server <- function(input,output,session){
    
    output$warn <- renderText({
        
        req(input$file1)
        
        inFile         <- input$file1
        
        data           <- read_excel(inFile$datapath, 1)
        
        peak           <- subset.data.frame(data, data$...3 == "Peak Hopping")[,c(1,2,4)]
        
        IDs            <- setNames(subset(data, data$`Sample Information` == "Sample ID:")[,2],"ID")
        IDlength       <- nrow(IDs)
        dup            <- unique(as.character(IDs$ID[duplicated(IDs$ID)]))
        
        ntest          <- setNames(as.data.frame(paste0(peak$`Sample Information`, peak$...2)), "ele")
        
        if(IDlength*length(unique(ntest$ele)) != nrow(peak)){
        stop("Different number of elements anlysed in the different samples ??! The same number of analysed elements in each sample is needed.")}
        
        if(IDlength != length(unique(IDs$ID))){paste(c("The sample names`", dup, "`are duplicated (not unique). You can change this by placing a number or letter behind the sample name. If not, then only the first sample is given."))}
        else(NULL)})
    
    reactiveDF<-reactive({
        
        req(input$file1)
        
        inFile          <- input$file1
        
        data            <- read_excel(inFile$datapath, 1)

        #Select only the peak hopping intensity
        peak           <- subset.data.frame(data, data$...3 == "Peak Hopping")[,c(1,2,4)]
        
        #Select the number of samples analysed
        IDs            <- setNames(subset(data, data$`Sample Information` == "Sample ID:")[,2],"ID")
        IDlength       <- nrow(IDs)
        
        #Set the names of the elements that were analysed
        nElement       <- setNames(as.data.frame(paste0(peak$`Sample Information`, peak$...2)), "Element")
        nrElements     <- length(unique(nElement$Element))
        
        #Create IDs in order of samples
        repID <- character()
        for(i in c(IDs$ID)){
            repID <- c(repID, rep(i, nrElements))}
        
        
        #Combine ID number, with the analysed element and corresponding intensity of the peak
        comb                   <- cbind.data.frame(repID, nElement, peak[-c(1,2)])
        colnames(comb)[c(1,3)] <- c("ID", "Peak")
        comb$Peak              <- as.numeric(comb$Peak)
        
        #organise data
        fin            <- suppressWarnings(reshape(comb, idvar = "ID", timevar = "Element", direction = "wide"))
        colnames(fin)  <- do.call(rbind, strsplit(colnames(fin), split='.', fixed=TRUE))[,2]
        rownames(fin)  <- NULL
        
        return(fin)
        
    })
    
    output$mytable = DT::renderDataTable({
        req(input$file1)
        
        return(DT::datatable(reactiveDF(),  options = list(pageLength = 10), filter = c("top")))
    })
    
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".xlsx", sep="")
        },
        content = function(file) {
            write.xlsx(reactiveDF(), file, row.names = FALSE)
        }
    )
    
    output$downloadExample <- downloadHandler(
        filename = function() {
            paste("Example-", Sys.Date(), ".xlsx", sep="")
        },
        content = function(file) {
            write.xlsx(Example, file, row.names = FALSE)
        }
    )
    
}

shinyApp(ui,server)
