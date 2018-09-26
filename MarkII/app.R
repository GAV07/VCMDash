library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(tidyr)
library(forcats)
library(zoo)
library(stringr)


#R Script - TG
Metrics1 <- read.csv("Venture (Q1 2018).csv", stringsAsFactors = F)
Metrics1$Date <- as.Date(Metrics1$Date, "%m/%d/%Y")
monthSplit <- mutate(Metrics1, as.yearmon(Metrics1$Date))
monthSplit <- monthSplit %>% 
  tidyr::separate(Date, c("month", "year"), sep = " ")
monthSplit$month <- as.factor(monthSplit$month) 
monthSplit$month <- factor(monthSplit$month, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
Newbie <- sum(Metrics1$First.time)/sum(Metrics1$Total.Guest.Visits)
Casual <- sum(Metrics1$X2.to.4)/sum(Metrics1$Total.Guest.Visits)
Veteran <- sum(Metrics1$X5..times)/sum(Metrics1$Total.Guest.Visits)

#R Script - PP
PP <- read.csv("PP.csv", stringsAsFactor = F)
PP$Gender <- as.factor(PP$What.is.the.gender.of.your.lead.presenter.)
PP$Gender <- fct_collapse(PP$Gender, Non_Conforming = c("Male, Female", "Gender non-conforming", "Female, Gender non-conforming", "Male, Female, Gender non-conforming", "Male, Female, Gender non-conforming", "None of the above"))
PP$ST <- as.factor(PP$Session.Type)
PP$ST <- fct_collapse(PP$ST, LEARN = c("LEARN", "LEARN"),
                      CUT = c("No", "Yes", ""))
PP$ST <- fct_infreq(PP$ST)
PP$Ethnicity <- as.factor(PP$What.is.the.ethnicity.of.your.lead.presenter.)
PP$Ethnicity <- fct_collapse(PP$Ethnicity, Multicultural = c("Asian/ Pacific Islander, Black or African American, Hispanic or Latino",                   
                                                             "Asian/ Pacific Islander, Black or African American, Hispanic or Latino, White (Caucasian)",
                                                             "Asian/ Pacific Islander, European; African",                                               
                                                             "Asian/ Pacific Islander, Hispanic or Latino, White",                                       
                                                             "Asian/ Pacific Islander, White",
                                                             "Black or African American, Caribbean ",                                                    
                                                             "Black or African American, Haitian",                                                       
                                                             "Black or African American, Hispanic or Latino",                                           
                                                             "Black or African American, Hispanic or Latino, White",                                     
                                                             "Black or African American, Hispanic or Latino, White (Caucasian)",                         
                                                             "Black or African American, White (Caucasian)",
                                                             "everyone",                                                                                 
                                                             "everything",
                                                             "Hispanic or Latino, Native American or American Indian",                                   
                                                             "Hispanic or Latino, White",                                                                
                                                             "Hispanic or Latino, White (Caucasian)",
                                                             "Multi",                                                                                    
                                                             "multi-ethnic",                                                                            
                                                             "Multiple",                                                                                 
                                                             "multiple genders/ethnicities",                                                             
                                                             "multiple presenters, several uncomfirmed thus far",
                                                             "white; Latino; Black (multiple presenters)",
                                                             "We have multi-cultural presentation team.",
                                                             "Multicultural",
                                                             "Eastern European"),
                             Other = c("How does one answer this question?", "N/A", "NA!", "n/a (documentary)", "I do not know", "it will be a few of us ladies!" ),
                             White = c("White", "White, N/A", "White (Caucasian)", "There is a Caucasian woman and European man ")
)
PP$Industry <- as.factor(PP$Industry)
PP$Industry <- fct_collapse(PP$Industry, Arts = c( "Arts & Creative Design",                                                                                           
                                                   "Arts and Creative Design",                                                                                         
                                                   "Arts and Creative Design, Education and Science, Entrepreneurial Support Organization",                            
                                                   "Arts and Creative Design, Manufacturing/ Fabrication",                                                             
                                                   "Arts and Creative Design, Media "),
                            NonProfit = c( "Non profit",                                                                                                       
                                           "Non Profit",                                                                                                       
                                           "Non Profit  ",   
                                           "Several Non-Profits",
                                           "Non- Profit Youth Development",                                                                                    
                                           "Non-Porift & Women Organizations" ,                                                                                
                                           "non-profit",                                                                                                       
                                           "Non-profit",                                                                                                       
                                           "Non-Profit",                                                                                                       
                                           "Non-Profit/Social Justice",                                                                                        
                                           "Non-Profits",                                                                                                      
                                           "Nonprofit",                                                                                                        
                                           "Not for Profit",
                                           "NonProfit",
                                           "nonprofits & social ventures"), 
                            Technology = c("Tech Startups",                                                                                                    
                                           "tech-enabled, professional service",                                                                               
                                           "Tech/Entrepreneurship ",                                                                                           
                                           "technology", 
                                           "Architecture & Technology",
                                           "Technology",                                                                                                       
                                           "Technology + Innovation ",                                                                                         
                                           "Technology and Consumer ",
                                           "Clean, Green, Alternative Energy & Technology",
                                           "Drones",
                                           "Information Technology",
                                           "Technology, Life Sciences and Health Care, Entrepreneurial Support Organization" ),
                            Government = c("Government"),
                            Healthcare = c("Healthcare",
                                           "Health & Wellness",
                                           "Life Sciences & Health Care",
                                           "Life Sciences and Health Care"),
                            Science = c("Science"),
                            Energy = c("Energy"),
                            Marketing = c("Marketing",
                                          "Marketing / Business",
                                          "Marketing & Advertising"),
                            Business = c("Business",
                                         "any business",
                                         "Businesses",
                                         "Real Estate, Restaurant, Film, ",
                                         "Small Business",
                                         "Small and Medium  Business",
                                         "Social Enterprise",
                                         "Minority-owned businesses",
                                         "Business (startup & small biz)",
                                         "Business Development",
                                         "Business Owners",
                                         "business owners, entrepreneurs, sales and engagement teams",
                                         "Business, Law & Immigration",
                                         "International Exchange"),
                            Finance = c("Banking & Finance"),
                            Entrepenurship = c( "start-up companies",                                                                                               
                                                "Start-up- (General)",                                                                                              
                                                "Startup, Tech, Business",                                                                                          
                                                "Startups",
                                                "Entreprenuership",
                                                "Several. Entrepreneurship focus.",
                                                "Social Entrepreneurship",
                                                "Entreperneuship and small businesses",
                                                "startups/small business/innovation",
                                                "Entrepreneurs & Business owners",
                                                "Education",                                                                                                       
                                                "Education and Science",                                                                                            
                                                "Enterprenuers",                                                                                                
                                                "Entrepreneurial Support Organization",                                                                             
                                                "Entrepreneurs",                                                                                                    
                                                "Entrepreneurs ",                                                                                                   
                                                "Entrepreneurs, For Profit, Non-Profit Companies" ,                                                                 
                                                "Entrepreneurship",                                                                                                 
                                                "ENTREPRENEURSHIP",                                                                                                
                                                "Entrepreneurship ",                                                                                               
                                                "Entrepreneurship and Startups",                                                                                   
                                                "Entrepreneurship/ All Industries ",
                                                "innovation",                                                                                                       
                                                "Innovation & Creative Problem-Solving",
                                                "Startups/Entrepreneurs"),
                            Civic = c("Community ",                                                                                                       
                                      "Community Building",
                                      "civic engagement",
                                      "Food ",
                                      "corporate social responsibility",
                                      "Economic Development",
                                      "community building, entrepreneurship",                                                                             
                                      "Community Engagement",                                                                                             
                                      "Community Initiatives", 
                                      "Civic Engagement",
                                      "Civic Engagement, Policy and Advocacy",
                                      "Community Redevelopment", 
                                      "Civic Innovation",
                                      "Philanthropy",
                                      "Human services (gov and nonprofit)",
                                      "Social Justice / Innovation Community Engagement ",
                                      "Youth development, mentoring and sports" ,                                                                         
                                      "Youth development, mentoring, and sports",
                                      "Housing",                                                                                                         
                                      "Housing, homeownership and land security",                                                                         
                                      "Housing, homeownership and land security ",
                                      "Social ",                                                                                                          
                                      "social entrepreneurship",                                                                                          
                                      "Social Justice Advocacy",                                                                                          
                                      "Social Service",
                                      "Advocacy -- Immigration",
                                      "Urban mobility, placemaking, and design"),
                            Professional_Services = c("Professional Services",                                                                                            
                                                      "Professional Services, Entrepreneurial Support Organization", 
                                                      "Personal Branding",
                                                      "Human Resources, Career Development",
                                                      "Accelerator is industry agnostic. Industries covered by participants: Consumer Goods, Health & Beauty, Tech, etc.",
                                                      "Chamber of Commerce",
                                                      "Mentors/Support for Returning Citizens",
                                                      "Personal Development",
                                                      "Product Development & IP ",
                                                      "public speaking",
                                                      "Management & Organization Behavior",
                                                      "Design, Software, Sustainable Production",
                                                      "Fintech, branded content, localization systems, european internships management"),
                            Sports = c("Sports"),
                            Media = c("Media",
                                      "Media & Communications",
                                      "Journalism",
                                      "Blogging/Networking"),
                            Legal = c("Legal",
                                      "LEGAL",
                                      "Legal ",
                                      "Legal/non-profit"),
                            Investing = c("Venture Capital",
                                          "Angel Investing",
                                          "Entrepreneur Funding"),
                            Hospitality = c("Culture & Tourism" ),
                            Other = c( "all",                                                                                                              
                                       "All",                                                                                                              
                                       "ALL", 
                                       "All ",
                                       "Arts, Education, Tech, Healthcare",
                                       "All - Women; Entrepreneurs, Employees, Citizens",
                                       "All industries",
                                       "all tech startups",
                                       "Everything",
                                       "All Entrepreneurs",                                                                                                
                                       "All industries ",                                                                                                  
                                       "All Industries ", 
                                       "We are focusing on Education, Technology, Finance and Sustainability",
                                       "TBD by selection process. But most likely arts, education, tech, healthcare",
                                       "All Miamians, regardless of industry",                                                                             
                                       "All of the above",
                                       " All that require Creativity & Innovation",
                                       "any" ,                                                                                                             
                                       "Any are welcome" ,                                                                                                 
                                       "Any business interested in growing their online presence.",                                                        
                                       "Any profession",                                                                                                   
                                       "Any that uses communication as a skill",                                                                           
                                       "applies to all industries", 
                                       "Milennials",
                                       "Multi-sectoral",
                                       "n/a",
                                       "Agriculture, Immigration and Education",
                                       "N/A",
                                       "Diverse topcis",
                                       "Various",
                                       "Innovation",
                                       "Mixed: Education, Life & Health Care, Technology, and Arts & Creative Design"))
PP$Industry <- fct_infreq(PP$Industry)

#VCM Demographics
VCM <- read.csv("Visitors (Q1 2018).csv", sep = "\t")
drop <- c("visitor_id", "first_name", "last_name", "email", "visitor_type", "affiliate", "newsletter_subcription", "Survey created at", "Survey modified at"  )
VCM <- VCM[ , !(names(VCM) %in% drop)]
VCM <- VCM[VCM$has_filled_survey == "Yes",]
colnames(VCM) <- c("V-Count", "Opt-In?", "Survey?", "Primary Role", "Biz Stage", "Gender", "C-Country", "Zip", "C-City", "O-Country", "O-City", "Age", "B-Day", "Asian", "Black", "Hispanic", "White", "Other", "Timestamp1", "Timestamp2")

VCM$Zip <- as.factor(VCM$Zip)
VCM$`O-City`<- as.factor(VCM$`O-City`)
VCM$`O-Country` <- as.factor(VCM$`O-Country`)
VCM$`O-Country` <- fct_lump(VCM$`O-Country`, other_level = "Other", n = 10)
VCM$Age<- as.factor(VCM$Age)
VCM$Age <- factor(VCM$Age, exclude = c(NA, "null"))
VCM$`Primary Role` <- as.factor(VCM$`Primary Role`)
VCM$`Primary Role` <- factor(VCM$`Primary Role`, exclude = "null")
VCM$`Primary Role` <- fct_lump(VCM$`Primary Role`, other_level = "Other", n = 5)
VCM$`Biz Stage` <- as.factor(VCM$`Biz Stage`)
VCM$`Biz Stage` <- fct_lump(VCM$`Biz Stage`, other_level = "Other", n = 7)
VCM$Ethnicity <- paste(VCM$White, VCM$Black, VCM$Other, VCM$Asian, VCM$Hispanic)
VCM$Ethnicity <- gsub("NA", "", VCM$Ethnicity)
VCM$Ethnicity <- str_trim(VCM$Ethnicity)
VCM$Ethnicity <- fct_collapse(VCM$Ethnicity, MultiCultural = c("Caucasian / White    Hispanic / Latino",
                                                               "Caucasian / White African American / Black",
                                                               "Caucasian / White  Hispanic / Latino",
                                                               "Caucasian / White Hispanic / Latino",
                                                               "Caucasian / White   Asian/Pacific Islander",
                                                               "nativeamerican/mixed",
                                                               "African American / Black Native American",
                                                               "African American / Black   Hispanic / Latino",
                                                               "Asian/Pacific Islander Hispanic / Latino",
                                                               "Brazilian",
                                                               "Moroccan",
                                                               "Asian/Pacific Islander",
                                                               "Asian/Pacific Islander Hispanic or Latino",
                                                               "mixed",
                                                               "bi racial",
                                                               "arabic",
                                                               "Caribbean decent",
                                                               "Haitian and Arabic",
                                                               "Brazilian,Palestinian,French",
                                                               "Portuguese",
                                                               "Mestizo  Hispanic / Latino",
                                                               "Mixed",
                                                               "Middle Eastern",
                                                               "Jamaican/German",
                                                               "European",
                                                               "Multiethnic (Hispanic & White)",
                                                               "Caucasian / White African American / Black Native American",
                                                               "Caucasian / White Asian/Pacific Islander"),
                                              Other = c("",
                                                        "Cool",
                                                        "aboriginal australian",
                                                        "european",
                                                        "Greek",
                                                        "Human",
                                                        "Human  Hispanic / Latino",
                                                        "human",
                                                        "NYOB",
                                                        "Student",
                                                        "multiple",
                                                        "Sorry, I just don't think in those terms",
                                                        "Sorry"),
                                              Jewish = c("Jewish",
                                                         "Hebrew Israelite",
                                                         "jewish"),
                                              No_Answer = c(""),
                                              White = c("white",
                                                        "Caucasian / White"),
                                              Black = c("African American / Black",
                                                        "African American / Black Haitian",
                                                        "Jamiacan",
                                                        "Black Haitian",
                                                        "african",
                                                        "afro cuban american latinX"))

#Pre-Render Graphs
plot3 <- ggplot(PP, aes(x = PP$Gender)) +
  geom_histogram(stat = "count", fill = "darkturquoise") +
  theme_classic() + 
  labs(title="Programming Partners") +
  ggtitle("Programing Partners' Gender") +
  xlab("") +
  ylab("") + 
  theme_minimal(base_size = 15) +
  geom_text(aes(y = prop.table(..count..) * 250, 
                label = paste0(round(prop.table(..count..) * 100), '%')), 
            stat = 'count', 
            position = position_dodge(1), 
            size = 5, color="white") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position="none",
        plot.title=element_text(family="Times", face="italic", size=14),
        plot.subtitle=element_text(family="Times", face="italic", size=15),
        axis.text.x = element_text(hjust = 1, size = 7),
        axis.text.y = element_blank())

plot4 <- ggplot(PP, aes(fct_rev(fct_infreq(PP$Ethnicity, ordered = T)))) +
  geom_histogram(stat = "count", fill = "darkturquoise") +
  theme_classic() + 
  coord_flip() +
  ggtitle("Programing Partners' Ethnicity") +
  xlab("") +
  ylab("") + 
  theme_minimal(base_size = 9) +
  geom_text(aes(y = prop.table(..count..) * 250, 
            label = paste0(round(prop.table(..count..) * 100), '%')), 
            stat = 'count', 
            position = position_dodge(1), 
            size = 3, color="white") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position="none",
        axis.text.x = element_blank())

plot5 <- ggplot(PP, aes(PP$ST)) +
  geom_histogram(stat = "count", fill = "darkturquoise") +
  theme_classic() + 
  ggtitle("Session Type") +
  xlab("") +
  ylab("") + 
  theme_minimal(base_size = 20) +
  geom_text(aes(y = prop.table(..count..) * 250, 
                label = paste0(round(prop.table(..count..) * 100), '%')), 
            stat = 'count', 
            position = position_dodge(1), 
            size = 5, color="white") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position="none",
        plot.title=element_text(family="Times", face="italic", size=14),
        plot.subtitle=element_text(family="Times", face="italic", size=15),
        axis.text.x = element_text(hjust = 1, size = 7),
        axis.text.y = element_blank())

plot6 <- ggplot(PP, aes(fct_rev(fct_infreq(PP$Industry, ordered = T)), y = (..count..)/sum(..count..))) + 
  geom_histogram(stat = "count", fill = "darkturquoise") + 
  coord_flip() +  
  ggtitle("Programing Partners' Industry") +
  xlab("") +
  ylab("") + 
  scale_y_continuous(labels = scales::percent) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.major.y = element_blank(),
        legend.position="none",
        plot.title=element_text(family="Times", face="italic", size=14),
        axis.text.x = element_text(hjust = 1, size = 9),
        axis.text.y = element_text(hjust = 1, size = 9))

plot7 <- ggplot(VCM, aes(fct_infreq(VCM$`Primary Role`, ordered = T))) + 
  geom_bar(aes(y = (..count..)/sum(..count..)*100), fill = "darkturquoise") +
  xlab("") +
  ylab("") + 
  ggtitle("Type of Innovators") +
  theme_minimal(base_size = 7) +
  scale_x_discrete(labels = c("Startup", "Corporate", "Service Provider", "Student", "Investor", "Other"))+
  geom_text(aes(y = prop.table(..count..) * 50, 
                label = paste0(round(prop.table(..count..) * 100), '%')), 
            stat = 'count', 
            position = position_dodge(1), 
            size = 5, color="white") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position="none", 
        plot.title=element_text(family="Times", face="italic", size=14),
        axis.text.x=element_text(vjust = .6, size=7),
        axis.text.y= element_blank())

plot8 <- ggplot(VCM, aes(fct_rev(fct_infreq(VCM$Ethnicity, ordered = T)))) + 
  geom_bar(aes(y = (..count..)/sum(..count..)*100), fill = "darkturquoise") +
  coord_flip() +
  ggtitle("Ethnicity of Innovators") +
  xlab("") +
  ylab("") + 
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        legend.position="none",
        plot.title=element_text(family="Times", face="italic", size=14),
        plot.subtitle=element_text(family="Times", face="italic", size=15),
        axis.text.x = element_text(hjust = 1, size = 7),
        axis.text.y = element_text(hjust = 1, size = 7))

plot9 <- VCM %>%
  filter(Age == c("31 - 40", "25 - 30", "19 - 24", "41 - 50", "51 - 60", "60+", "under 18")) %>%
  ggplot(aes(fct_infreq(Age, ordered = T))) + 
  geom_bar(aes(y = (..count..)/sum(..count..)*100), fill = "darkturquoise") +
  xlab("") +
  ylab("") + 
  ggtitle("Age of Innovators") +
  theme_minimal(base_size = 7) +
  geom_text(aes(y = prop.table(..count..) * 50, 
                label = paste0(round(prop.table(..count..) * 100), '%')), 
            stat = 'count', 
            position = position_dodge(1), 
            size = 5, color="white") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position="none", 
        plot.title=element_text(family="Times", face="italic", size=22),
        plot.subtitle=element_text(family="Times", face="italic", size=15),
        axis.text.x=element_text(vjust = .6, size=7),
        axis.text.y=element_blank())

plot10 <-  VCM %>%
  dplyr::filter(`Biz Stage` == c("Concept", "Funded", "Seed", "Growth", "Mature")) %>%
  ggplot(aes(fct_infreq(`Biz Stage`, ordered = T))) + 
  geom_bar(aes(y = (..count..)/sum(..count..)*100), fill = "darkturquoise") +
  xlab("") +
  ylab("") +
  #scale_x_discrete(labels = c("No Answer", "N/a", "Growth", "Concept", "Seed", "Mature", "Funded", "Other")) +
  ggtitle("Business Stage of Innovators") +
  theme_minimal(base_size = 7) +
  geom_text(aes(y = prop.table(..count..) * 50, 
                label = paste0(round(prop.table(..count..) * 100), '%')), 
            stat = 'count', 
            position = position_dodge(1), 
            size = 4, color="white") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position="none", 
        plot.title=element_text(family="Times", face="italic", size=14),
        plot.subtitle=element_text(family="Times", face="italic", size=15),
        axis.text.x=element_text(vjust = .6, size=7),
        axis.text.y=element_blank())

#################################################################################
#Shiny App
shinyApp(
  ui = dashboardPage( skin = "black",
    dashboardHeader(
      title = "Venture Cafe Miami MarkII"
    ),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Thrusday Gathering", tabName = "tg"),
        menuItem("Programing Partners", tabName = "pp"),
        menuItem("Visitor Demographics", tabName = "v")
      )
    ),
    dashboardBody( 
      tags$head(tags$style(HTML('
        .main-header .logo {
                        font-family: "Georgia", Times, "Times New Roman", serif;
                        font-weight: bold;
                        font-size: 24px;
                        background-color: "white"; }
                                '))),
      tabItems(
        tabItem(tabName = "tg",
      fluidRow(
      valueBox(8292, "Total Number of Subscribers", icon = icon("list"), color = "teal"),
      valueBox(12812, "Total Number VC-Goers", icon = icon("fa fa-user-circle"), color = "teal"),
      valueBox(654, "Total Number Programs", icon = icon("fa fa-handshake-o"), color = "teal")
      ),
      
      fluidRow(
        column(6, plotlyOutput("plot1") ),
        column(6, plotlyOutput("plot11") )
      )),
     tabItem(tabName = "pp",
             fluidRow(
               plotlyOutput("plot3"),
               plotlyOutput("plot4"),
               plotlyOutput("plot5"),
               plotlyOutput("plot6")
             )),
     tabItem(tabName = "v",
             fluidRow(
               plotlyOutput("plot7"),
               plotlyOutput("plot8"),
               plotlyOutput("plot9"),
               plotlyOutput("plot10")
             ))
    )
   )
  ),
  
  server = function(input, output) { 
    
    output$plot1 <- renderPlotly({
      plot_ly(Metrics1, x = ~Date, y = ~Total.Guest.Visits, name = "Total", type = "scatter", mode = 'lines+markers', 
              line = list( width = 1.4, opacity = .5, color = "darkturquoise"),
              marker = list( size = 3.5, color = "darkturquoise")) %>%
        layout(annotations = list(x = "2016-09-22", y = 350, showarrow = F, text = "<i>Thursday Gathering Total<br>Week to Week<i>")
               , xaxis = list(title = "", showgrid=F), yaxis = list(title = ""))
      
    })
    
    output$plot11 <- renderPlotly({
      plot_ly(Metrics1, x = ~Date, y = ~First.time, name = 'First Timers', mode = "lines+markers", 
              line = list( width = 1.4, opacity = .5),
              marker = list( size = 4.5)) %>%
        add_trace(y = ~First.time, name = 'First Timers', mode = "lines+markers") %>%
        add_trace(y = ~X2.to.4, name = '2-4x', mode = "lines+markers") %>%
        add_trace(y = ~X5..times, name = 'Veterans', mode = "lines+markers") %>%
        layout(annotations = list(x = "2016-09-22", y = 350, showarrow = F, text = "<i>Thursday Gathering<br>Breakdown<i>")
               , xaxis = list(title = "",showgrid=F), yaxis = list(title = ""))
    })
    
    
    output$plot3 <- renderPlotly ({
     ggplotly(plot3) %>%
        style(hoverinfo = "skip")
    })
    
    output$plot4 <- renderPlotly ({
      ggplotly(plot4) 
    })
    
    output$plot5 <- renderPlotly ({
      ggplotly(plot5)
    })
    
    output$plot6 <- renderPlotly({
      ggplotly(plot6) %>%
        style(hoverinfo = "skip")
    })
    
    output$plot7 <- renderPlotly({
      ggplotly(plot7) %>%
        style(hoverinfo = "skip")
    })
    
    output$plot8 <- renderPlotly({
      ggplotly(plot8) %>%
        style(hoverinfo = "skip")
    })
    
    output$plot9 <- renderPlotly({
      ggplotly(plot9) %>%
        style(hoverinfo = "skip")
    })
    
    output$plot10 <- renderPlotly({
      ggplotly(plot10) %>%
        style(hoverinfo = "skip")
    })
  }
)
