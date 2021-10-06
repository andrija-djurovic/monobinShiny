ui <- dashboardPage(skin = "black", title = "Shiny user interface for monobin package", 

dashboardHeader(title = HTML("Shiny user interface for 
				     <a href = 'https://cran.r-project.org/web/packages/monobin/monobin.pdf'>
				     monobin</a> package"),
		    titleWidth = "500px"),

dashboardSidebar(disable = TRUE),

dashboardBody(

	dm.ui(id = "data.manager"),
	di.ui(id = "desc.imputation"),
	mb.ui(id = "monobin")
)
)
