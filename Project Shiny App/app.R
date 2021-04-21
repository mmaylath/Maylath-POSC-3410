# Title: Shiny App for Project POSC 3410
# Author: Madeleine Maylath
# Date: 4/20/21


library(shiny)
library(tidyverse)
load("app_workspace_final.RData")

# UI ####
ui <- fluidPage(
    # Title Panel 
    titlePanel("Impact of education and income on views of space program funding"),
    # Side bar conrtrols 
    sidebarLayout(
        sidebarPanel(
            h3("Impact of education on views of space"),
            checkboxGroupInput(
                "incomes",
                "Select the income bracket for analysis (pick 1):",
                c("$1000 to 2999", "$15000 - 19999", "$7000 to 7999", 
                  "$8000 to 9999",  "$20000 - 24999", "$4000 to 4999",      
                  "$10000 - 14999", "$25000 or more", "$3000 to 3999",  "Lt $1000",      
                  "$5000 to 5999",  "$6000 to 6999"),
                c("$20000 - 24999")
            ),
            h3("Impact of income on views of space"),
            sliderInput(
                "years_educ",
                "Select interval for years of education for analysis:",
                min=min(analysis_df$EDUC, na.rm=TRUE),
                max=max(analysis_df$EDUC, na.rm=TRUE),
                value=c(min(analysis_df$EDUC, na.rm=TRUE), max(analysis_df$EDUC, na.rm=TRUE)),
                sep=""
            ),
            
        ),
        
        #Main panel
        mainPanel(
            h1("Analyses"),
            h3("Purpose:"),
            p("The purpose of this app is to demonstrate different relationships. These are: 1) The relationship between having a college education and supporting increased space funding, 2) The relationship between years of education and supporting
              increased space funding, 3) The relationship between having an income above the poverty line and supporting increased space funding, and 4) The relationship between real income and supporting increased space funding."),
            br(),
            h3("Theory"),
            p("The theory being examined is that educational attainment and income have different effects on a person’s views of space exploration. Different hypotheses were tested exploring the relationship between educational attainment and support for space
              and between income and support for space funding."),
            h3("Hypotheses: Null hypotheses are listed for each figure/part of the analysis in the app."),
            p("H0,1:There is no relationship between support for increased space funding and educational attainment."),
            p("Ha,1: There is a relationship between support for increased space funding and educational attainment."),
            br(),
            p("H0,2: There is no relationship between support for increased space funding and years of education."),
            p("Ha,2: There is a relationship between support for increased space funding and years of education. "),
            br(),
            p("H0,3: There is no relationship between support for increased space funding and income."),
            p("Ha: There is a relationship between support for increased space funding and income."),
            br(),
            p("H0,4: There is no relationship between support for increased space funding and income."),
            p("Ha,4: There is a relationship between support for increased space funding and income."),
            br(),
            p("For each analysis, a binary variable was created for support for space funding (the variable NATSPAC in the GSS).
               A value of 1 meant the respondent answered 'Too little' in response to how they view the government's spending
               on the space program, and a value of 0 meant the respondent answered 'Too much' in response to the question. This binary variable is plotted on the
              y-axis. A value of 1 indicates a 100% probability they support increased funding, while a value of 0 means a 0% probability they support increased funding for the space program."),
            h2("Part 1: Impact of education on views of space"),
            h3("Hypothesis 1"),
            h4("Fig. 1: Impact of college education on views of space"),
            plotOutput("Coll_bin"),
            p("This plot shows the probability of supporting space exploration with respect to having gone to college. On the x-axis is a binary variable
              describing whether a person went to college or not: 1 means they did, 0 means they did not. One the y-axis is a binary variable describing whether the respondent
              supports increased funding for the space program. In this plot, income is controlled for by selecting
              an income range on the side panel, which allows effects of income/Socioeconomic status(SES) to be separated from impacts of education on a person's views
              of space funding."),
            br(),
            h4("Slope"),
            textOutput("slope1"),
            br(),
            h4("Intercept"),
            textOutput("int1"),
            br(),
            p("Interpretation of results: For every 100% increase in probability of a person having gone to college, there is a [slope*100]% increase in probability that
              the person will support increased funding for the national space program."),
            p("For each income bracket, the line has a positive slope. This indicates that controlling for income, there is an indication that increased probability of attending college
              increases the probability of a respondent supporting increased funding for the space program."),
            br(),
            h3("Hypothesis 2"),
            h4("Fig. 2: Impact of years of education on views of space"),
            plotOutput("yrs_of_educ"),
            p("This plot shows the probability of supporting space exploration with respect to years of education. On the x-axis is years of education. On the y-axis
              is a binary variable describing whether or not the respondent supported increased funding for the space program. In this plot, the effects of income/SES are 
              again controlled for by controlling the income range variable."),
            br(),
            h4("Slope"),
            textOutput("slope2"),
            br(),
            h4("Intercept"),
            textOutput("int2"),
            br(),
            p("Interpretation of results: For every 1 year increase in a respondent's years of education, there is a [slope*100]% increase in the probability that they support
              increased funding for the national space program."),
            p("For each income bracket, the line has a positive slope, though the steepness of this slope may vary. This indicates that there is a positive correlation between years of
              education and probability of supporting increased space funding."),
            br(),
            h4("Part 1 conclusions: Based on the results from these two regressions - on the college binary and the years of education, each versus probability of supporting space funding - it can be concluded
               that increased educational attainment increases the likelihood of a respondent supporting increased space funding. The impacts of education are separated from the impacts of SES by 
               controlling for income bracket."),
            br(),
            h2("Part 2: Impact of income/socioeconomic status (SES) on support for national space program"),
            h3("Hypothesis 3"),
            h4("Fig. 3: Impact of income on views of space"),
            plotOutput("Inc_bin"),
            p("This plot shows the probability of supporting increased space funding with respect to having an income above the poverty line. For this regression, a binary variable was created to indicate if a person's income
              was above the poverty line. A 0 indicates their income is below the poverty line, and a 1 indicates their income is above the poverty line. For this test, the poverty line was considered to be $15000 based on varying
              estimates for the US based on family size. The income binary was graphed on the x-axis, with the binary variable for supporting space funding graphed on the y-axis."),
            br(),
            h4("Slope"),
            textOutput("slope3"),
            br(),
            h4("Intercept"),
            textOutput("int3"),
            br(),
            p("Interpretation of results: For every 100% increase in probability of a person having an income above the poverty line, the probability of them supporting increased funding for the space program increases by [slope*100]%."),
            p("For each education range, there is a positive slope. This means that regardless of years of education, there appears to be a positive correlation between probability of having an income above the poverty line and probability of supporting increased
              space funding."),
            br(),
            h3("Hypothesis 4"),
            h4("Fig. 4: Impact of income on views of space"),
            plotOutput("Income_real"),
            p("This plot shows the probability of supporting space exploration with respect to income."),
            h4("Slope"),
            textOutput("slope4"),
            br(),
            h4("Intercept"),
            textOutput("int4"),
            br(),
            p("Interpretation of results: For every $1 increase in a respondent's income, the probability of them supporting increased space funding increases by
              [slope*100] %."),
            p("For each interval selected on the 'years of education' slider, the slope of the line is positive. For some intervals, the line starts out steeper and
            tapers off towards the right side of the graph near where income reaches $30000. This indicates that a linear model isn't a perfect fit for the relationship bewteeen income
              and support for space funding. This phenomena could also indicate that several linear regressions are needed for different income ranges, or that income has less of an impact beyond $30000."),
            br(),
            h4("Part 2 Conclusions: There is a positive correlation between probability of supporting increased space funding and probability of having an income above the estimated poverty line, and there is 
               also a positive correlation between probability of supporting increased space funding and income. Even though a linear model does not fit these relationships perfectly, the linear regression always
               indicates a positive relationship. "),
            br(),
            h4("Next steps for research"),
            p("In the future, income per capita within a family should be studied. This could possibly lead to a better characterization of a person’s SES and could allow for a more accurate understanding of how SES impacts someone’s support for the national space program.
              Future research could also work to control beyond the demographics examined here. Possible demographic controls could include sex, age, and race."),
            h4("References"),
            p("Smith, Tom W., Davern, Michael, Freese, Jeremy, and Morgan, Stephen, General Social Surveys, 1972-2018 [machine-readable data file] /Principal Investigator, Smith, Tom W.; Co-Principal Investigators, Michael Davern, Jeremy Freese, and Stephen Morgan; Sponsored by National Science Foundation. --NORC ed.-- Chicago: NORC, 2018: NORC at the University of Chicago [producer and distributor]. Data accessed from the GSS Data Explorer website at gssdataexplorer.norc.org."),
        )
    )
    
)
# Server logic ####
server <- function(input, output, session) {

    output$Coll_bin <- renderPlot({
        # Fig. 1 plot - college binary, impact on NATSPAC ####
        analysis_df <- gss_df %>% # creates streamlined, filtered data frame to make working with data easier
            select(EDUC, NATSPAC, RINCOME, WTSSALL) %>% # selects columns of interest
            # get rid of rows with noviable answers in EDUC, NATSPAC, and RINCOME
            filter(EDUC != "No answer", EDUC != "Don't know", !is.na(EDUC)) %>%
            filter(NATSPAC != "Not applicable", NATSPAC != "No answer", NATSPAC != "Don't know", !is.na(NATSPAC)) %>% 
            filter(RINCOME != "Not applicable", RINCOME != "Refused", RINCOME != "No answer", !is.na(RINCOME), RINCOME != "Don't know") %>% 
            # control for income - select one income bracket
            filter(RINCOME == input$incomes[1]) %>%
            # change EDUC variable from character to numeric
            mutate(EDUC = as.numeric(as.character(EDUC)))
        
        # create data frame of respondents who went to college, filter by if they had more than 12 years of school
        college_df <- analysis_df %>% 
            filter(EDUC > 12) %>% 
            # create binary variable for if they went to college
            mutate(college_binary = 1)
        # create data frame of respondents who did not go to college, filter by if they had less than 12 years of school
        no_college_df <- analysis_df %>% 
            filter(EDUC <= 12) %>% 
            mutate(college_binary = 0)
        # combine college data frames
        college_no_college_df <- bind_rows(college_df, no_college_df)
        
        # create dataframe of respondents who want to increase funding for space
        natspac_agree_df <- analysis_df %>%
            filter(NATSPAC == "Too little") %>% 
            # create binary variable for if respondents support increase in space funding
            mutate(natspac_binary = 1)
        # create dataframe of respondents who do not want to increase space funding
        natspac_disagree_df <- analysis_df %>% 
            filter(NATSPAC == "Too much") %>% 
            mutate(natspac_binary = 0)
        # combine dataframes for supporters and non-supporters of increasing space funding
        natspac_combined_df <- bind_rows(natspac_agree_df, natspac_disagree_df)
        
        # combine college and natspac dataframes and eliminate any duplicate columns
        natspac_college_df <- left_join(college_no_college_df, natspac_combined_df, by = c("EDUC", "NATSPAC", "RINCOME", "WTSSALL"))
        
        # create glm and linear regression plot for how natspace relates to college
        natspac_college_df %>% 
            ggplot(aes(x=college_binary, y=natspac_binary)) + 
            geom_point()+
            geom_smooth(method = "glm", method.args = list(family = "binomial"(link="probit")))+
            ggtitle("Support for space funding vs. educational attainment")+
            xlab("Probability of having gone to college")+
            ylab("Probability of supporting space funding")
        
        # for people who have a college education, there is a higher probability that they support more funding for space
    })
    # GLM for fig 1
    model1 <- glm(data = natspac_college_df, natspac_binary ~ college_binary, family="binomial"(link="probit"))
    output$slope1 <- renderText({model1[[1]][2]})
    output$int1 <- renderText({model1[[1]][1]})
    
    # Figure 2: linear regression on yrs of educ. vs support for space ####
    output$yrs_of_educ <- renderPlot({
        analysis_df <- gss_df %>% # creates streamlined, filtered data frame to make working with data easier
            select(EDUC, NATSPAC, RINCOME, WTSSALL) %>% # selects columns of interest
            # get rid of rows with nonviable answers in EDUC, NATSPAC, and RINCOME
            filter(EDUC != "No answer", EDUC != "Don't know", !is.na(EDUC)) %>%
            filter(NATSPAC != "Not applicable", NATSPAC != "No answer", NATSPAC != "Don't know", !is.na(NATSPAC)) %>% 
            filter(RINCOME != "Not applicable", RINCOME != "Refused", RINCOME != "No answer", !is.na(RINCOME), RINCOME != "Don't know") %>% 
            # control for income - select one income bracket
            filter(RINCOME == input$incomes[1]) %>%
            # change EDUC variable from character to numeric
            mutate(EDUC = as.numeric(as.character(EDUC)))
        
        # create dataframe of respondents who want to increase funding for space
        natspac_agree_df <- analysis_df %>%
            filter(NATSPAC == "Too little") %>% 
            # create binary variable for if respondents support increase in space funding
            mutate(natspac_binary = 1)
        # create dataframe of respondents who do not want to increase space funding
        natspac_disagree_df <- analysis_df %>% 
            filter(NATSPAC == "Too much") %>% 
            mutate(natspac_binary = 0)
        # combine dataframes for supporters and non-supporters of increasing space funding
        natspac_educ_df <- bind_rows(natspac_agree_df, natspac_disagree_df)
        
        # combine college and natspac dataframes and eliminate any duplicate columns
        # natspac_college_df <- left_join(college_no_college_df, natspac_combined_df, by = c("EDUC", "NATSPAC", "RINCOME", "WTSSALL"))
        
        # create glm and linear regression plot for how natspace relates to college
        natspac_educ_df %>% 
            ggplot(aes(x=EDUC, y=natspac_binary)) + 
            geom_point()+
            geom_smooth(method = "glm", method.args = list(family = "binomial"(link="probit")))+
            ggtitle("Support for space funding vs. educational attainment")+
            xlab("Years of education")+
            ylab("Probability of supporting space funding")
        
    })
    
    model2 <- glm(data = natspac_educ_df, natspac_binary ~ EDUC, family="binomial"(link="probit"))
    output$slope2 <- renderText({model2[[1]][2]})
    output$int2 <- renderText({model2[[1]][1]})
    
    # Figure 3: Income binary impact on NATSPAC ####
    output$Inc_bin <- renderPlot({
        analysis_df <- gss_df %>% 
            select(EDUC, NATSPAC, RINCOME, WTSSALL) %>% 
            filter(EDUC != "No answer", EDUC != "Don't know", !is.na(EDUC)) %>% 
            filter(NATSPAC != "Not applicable", NATSPAC != "No answer", NATSPAC != "Don't know", !is.na(NATSPAC)) %>% 
            filter(RINCOME != "Not applicable", RINCOME != "Refused", RINCOME != "No answer", !is.na(RINCOME), RINCOME != "Don't know") %>% 
            mutate(EDUC = as.numeric(as.character(EDUC))) %>% 
            filter(EDUC >= input$years_educ[1] & EDUC <= input$years_educ[2])
        
        # create dataframe of respondents in income brackets below poverty line
        lowinc_df <- analysis_df %>% 
            filter(RINCOME != "$25000 or more", RINCOME != "$20000 - 24999", RINCOME != "$15000 - 19999") %>% 
            # create binary income variable, 0 denotes they are below the poverty line
            mutate(income_binary = 0)
        # create dataframe of respondents in income brackets above poverty line
        highinc_df <- analysis_df %>% 
            filter(RINCOME == "$25000 or more" | RINCOME == "$20000 - 24999" | RINCOME == "$1500 - 19999") %>% 
            # create binary income variable, 1 denotes they are above the poveryt line
            mutate(income_binary = 1)
        # combine dataframes for both income groups
        all_incomes_df <- bind_rows(lowinc_df, highinc_df)
        
        # create dataframe of respondents who want to increase space funding
        natspac_agree_df <- analysis_df %>%
            filter(NATSPAC == "Too little") %>% 
            # create binary variable for views on space funding
            mutate(natspac_binary= 1)
        # create dataframe of respondents who do not want to increase space funding
        natspac_disagree_df <- analysis_df %>% 
            filter(NATSPAC == "Too much") %>% 
            # create binary variable for views on space funding, 0 denotes they don't want to increase funding
            mutate(natspac_binary = 0)
        # combine dataframes of supporters and non-supporters of increasing space funding
        natspac_combined_df <- bind_rows(natspac_agree_df, natspac_disagree_df)
        
        # combine income data frame and natspac dataframe while removing duplicate columns
        natspac_income_df <- left_join(all_incomes_df, natspac_combined_df, by = c("EDUC", "NATSPAC", "RINCOME", "WTSSALL"))
        
        # create GLM linear regression plot
        natspac_income_df %>% 
            ggplot(aes(x=income_binary, y=natspac_binary)) + 
            geom_point()+
            geom_smooth(method = "glm", method.args = list(family = "binomial"(link="probit")))+
            xlab("Probability of having income above poverty line ($15000)")+
            ylab("Probability of supporting space funding")+
            ggtitle("Support for increased space funding vs. Income level")
    })
    
    model3 <- glm(data = natspac_income_df, natspac_binary ~ income_binary, family="binomial"(link="probit"))
    output$slope3 <- renderText({model3[[1]][2]})
    output$int3 <- renderText({model3[[1]][1]})
    
    # Figure 4: Income impact on NATSPAC ####
    output$Income_real <- renderPlot({
        analysis_df <- gss_df %>% 
            select(EDUC, NATSPAC, REALRINC, WTSSALL) %>% 
            filter(EDUC != "No answer", EDUC != "Don't know", !is.na(EDUC)) %>% 
            filter(NATSPAC != "Not applicable", NATSPAC != "No answer", NATSPAC != "Don't know", !is.na(NATSPAC)) %>% 
            filter(!is.na(REALRINC) & REALRINC > 0) %>% 
            mutate(EDUC = as.numeric(as.character(EDUC))) %>% 
            #filter(EDUC > 8 & EDUC < 13) # looking at ppl with high school education
            filter(EDUC >= input$years_educ[1] & EDUC <= input$years_educ[2]) # looking at ppl with college or higher education
        summary(analysis_df$REALRINC)
        
        # find IQR
        #21669 - 6625 # EDUC > 8 & EDUC < 13
        #32625-11103 # EDUC > 12: 21522
        
        # find upper bound for outliers
        #13242 + 1.5*15044 # EDUC > 8 & EDUC < 13
        #21010 + 1.5*21522 # EDUC > 12
        
        # filter for income outliers
        analysis_df %>% 
            # filter(REALRINC < 35809) # EDUC > 8 & EDUC < 13
            filter(REALRINC < 53294) # EDUC > 12
        
        # create dataframe of respondents who do want to increase space funding
        natspac_agree_df <- analysis_df %>%
            filter(NATSPAC == "Too little") %>% 
            # create binary variable for views on space funding, 1 indicates they support an increase
            mutate(natspac_binary= 1)
        # create dataframe of respondents who do not want to increase space funding
        natspac_disagree_df <- analysis_df %>% 
            filter(NATSPAC == "Too much") %>% 
            # create binary variable for views on space funding, 0 indicates they do not support an increase
            mutate(natspac_binary = 0)
        # combine dataframes for supporters and non-supporters of increased space funding
        natspac_rincome_df <- bind_rows(natspac_agree_df, natspac_disagree_df)
        
        # create GLM linear regression plot
        natspac_rincome_df %>% 
            ggplot(aes(x=REALRINC, y=natspac_binary)) + 
            geom_point()+
            geom_smooth(method = "glm", method.args = list(family = "binomial"(link="probit")))+
            ggtitle("Support for space funding vs. income")+
            xlab("Income [$]")+
            ylab("Proability of supporting space funding")
    })
    
    model4 <- glm(data = natspac_rincome_df, natspac_binary ~ REALRINC, family="binomial"(link="probit"))
    output$slope4 <- renderText({model4[[1]][2]})
    output$int4 <- renderText({model4[[1]][1]})
}

# Run the application 
shinyApp(ui = ui, server = server)
