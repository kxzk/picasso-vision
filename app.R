library(shiny)
library(readr)
library(DT)
library(shinydashboard)
library(googleAuthR)
library(RoogleVision)
library(shinycssloaders)
library(DT)


CLIENT.ID  <- "ENTER YOUR CLIENT ID"
CLIENT.SECRET  <- "ENTER YOUR CLIENT SECRET"
SERVICE.ACCOUNT  <- "PATH TO SERVICE ACCOUNT.json"


options("googleAuthR.webapp.client_id" = CLIENT.ID)
options("googleAuthR.webapp.client_secret" = CLIENT.SECRET)
options("googleAuthR.scopes.selected" = c("https://www.googleapis.com/auth/cloud-platform"))

service_token  <- gar_auth_service(json_file = SERVICE.ACCOUNT)


header  <- dashboardHeader(
	title = "Project: Picasso",
	titleWidth = 300
)

body  <- dashboardBody(

	fluidRow(

		column(width = 6,

			box(width = NULL,
			title = "Input Image",
			status = "danger",
			solidHeader = FALSE,
				imageOutput('image') %>% withSpinner()
			)
			
		),

		column(width = 6,

			box(width = NULL,
			title = "Google Cloud Vision API",
			status = "danger",
			p("The model takes an image and returns a dataframe of probabilities around image labels."),
			fileInput('image1', 'Select Image:',
				accept = c(".png", ".jpg", ".jpeg"))
			)
		),

		column(width = 12,

			box(width = NULL,
			solidHeader = FALSE,
			status = "danger",
				DT::dataTableOutput('image_data') %>% withSpinner()
			)

		)

	)
)

ui  <- dashboardPage(
	skin = "red",
	header,
	dashboardSidebar(disable = TRUE),
	body
)

server <- function(input, output) { 

	output$image  <- renderImage({
		if (is.null(input$image1)) {
			outfile  <- 'picasso_bull.jpeg'	
			content_type  <- 'image/jpg'
		} else {
			outfile  <- input$image1$datapath	
			content_type  <- input$image1$type
		}

		list(src = outfile,
			 contentType = content_type,
			 width = 500)
	}, deleteFile = FALSE)


	output$image_data  <- DT::renderDataTable({
	
		if (is.null(input$image1)) {
			df  <- readr::read_csv('bull_data.csv')		
			return(DT::datatable(df))
		} else {
			response  <- getGoogleVisionResponse(input$image1$datapath,
				feature = "LABEL_DETECTION", numResults = 20)
			df_res  <- as.data.frame(response)
			return(DT::datatable(df_res,
						extensions = 'Buttons',
						options = list(
							dom = 'Bfrtip',
							buttons = c('csv', 'excel')
						))
				)
		}
	
	})


}

shinyApp(ui, server)
