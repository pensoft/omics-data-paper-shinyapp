library(shiny)
library(XML)
library(xml2)
library(tableHTML)
library(DT)

rm(list=ls())

#processing biosamples is done separately because the function returns a data frame which needs to be rendered separately
process_biosamples = function(input){
    id= input$id[1] #get the id input by the user and send a request to ENA
    request = paste0("https://www.ebi.ac.uk/ena/browser/api/xml/", id)
    res = httr::content(httr::GET(request))
    biosamples_colnames = c("MIxS  field", "Value", "BioSample ID")
    sample_id = xml_text(xml_find_all(res, "//XREF_LINK/ID[../DB='ENA-SAMPLE']")) #getting all the sample ids and parsing them
    sample_ids = parse_multiple_ids(sample_id)
    if (length(sample_ids)>0){
        mixs_field = c() #declaring some of the variables (vectors) used for generating the data frame
        value = c()
        biosample_ids = c()
        biosamples_df = data.frame()
        for (s in 1:length(sample_ids)) { #iterating over each biosample id and sending requests to BioSample
            request = paste0("https://www.ebi.ac.uk/ena/browser/api/xml/", sample_ids[s])
            sample_res = httr::content(httr::GET(request))
            biosample_id = xml_text(xml_find_all(sample_res,"//SAMPLE/IDENTIFIERS/EXTERNAL_ID[@namespace='BioSample']"))
            if (length(biosample_id)>0){
                biosample_request = paste0("https://www.ebi.ac.uk/biosamples/samples/", biosample_id, ".xml")
                biosample_res = httr::content(httr::GET(biosample_request))
                biosample_xml = xml2::as_xml_document(biosample_res)
                properties = xml_children(biosample_xml) #getting all the mixs fields and their values
                counter = 0
                for (p in properties){
                    if (xml2::xml_has_attr(p, "class")){ #there is one child node which is not named "Property" but "Database"
                        property_class = xml2::xml_text(xml2::xml_find_all(p, "@class"))
                        mixs_field = c(mixs_field, property_class)
                        qualified_values = xml_children(as_xml_document(p))
                        values = xml_text(xml_find_all(qualified_values[1], "."))
                        value = c(value, values)
                        counter = counter+1
                    }
                }
                biosample_ids = c(biosample_ids, rep(biosample_id, counter)) #adding a third vector with the biosample id
            }
        }
        
        biosamples_df = data.frame(mixs_field, value, biosample_ids) #creating the data frame
        colnames(biosamples_df) = biosamples_colnames
    }else{
        biosamples_df = NULL
    }
    return(biosamples_df)
}

#function to obtain metadata from ENA and ArrayExpress
process_study = function(input){
    id= input$id[1]
    request = paste0("https://www.ebi.ac.uk/ena/browser/api/xml/", id)
    res = httr::content(httr::GET(request))
    keywords = c()
    doc = xmlTreeParse(res)
    root_name = xmlName(xmlRoot(doc)[[1]])
    if (root_name == "PROJECT"){
        title = xml_text(xml_find_all(res, paste0("//", root_name, "/TITLE")))
    } else if (root_name == "STUDY"){
        title = xml_text(xml_find_all(res, paste0("//", root_name, "_TITLE")))
    } else{
        title = xml_text(xml_find_all(res, paste0("//", root_name, "TITLE")))
    }
    abstract = xml_text(xml_find_all(res,  paste0("//", root_name, "_ABSTRACT")))
    if (length(abstract)<1){
        abstract = xml_text(xml_find_all(res, "//DESCRIPTION"))
    }
    
    #getting the data resources
    fastq_files = xml_text(xml_find_all(res,"//XREF_LINK/ID[../DB='ENA-FASTQ-FILES']"))
    ena_run_id = xml_text(xml_find_all(res, "//XREF_LINK/ID[../DB='ENA-RUN']"))
    res_fastq = httr::content(httr::GET(fastq_files))
    res_fastq = strsplit(res_fastq, "\n")
    res_fastq = res_fastq[[1]]
    res_fastq = res_fastq[2:length(res_fastq)]
    res_links = c()
    res_identifiers = c()
    counter = 0
    data_resources = c()
    for (r in 1:length(res_fastq)){
        components = strsplit(res_fastq[r], "\t")
        identifier = components[[1]][1]
        link = components[[1]][2]
        links = strsplit(link, ";")
        link_1 = links[[1]][1]
        link_2 = links[[1]][2]
        data_resource = paste("<h3 style=\"color:DARKCYAN;\">Resource ", r, "</h3>", "<h4 style=\"color:LIGHTSEAGREEN;\">Download URL</h4>", link_1, "\n", link_2,  "<h4 style=\"color:LIGHTSEAGREEN;\">Resource identifier</h4>", identifier, "<h4 style=\"color:LIGHTSEAGREEN;\">Data format</h4>", "FASTQ")
        data_resources = c(data_resources, data_resource) 
    }
    dr = paste(data_resources, sep = " ", collapse = "</br>")
    sample_id = xml_text(xml_find_all(res, "//XREF_LINK/ID[../DB='ENA-SAMPLE']"))
    experiment_id =  xml_text(xml_find_all(res, "//XREF_LINK/ID[../DB='ENA-EXPERIMENT']"))
    arrayexpress_id = xml_text(xml_find_all(res,  paste0("//", root_name,"/@alias[../@broker_name='ArrayExpress']")))
    #getting ArrayExpress metadata if it exists
    if (length(arrayexpress_id)>0){
        request = paste0("https://www.ebi.ac.uk/arrayexpress/xml/v3/experiments/", arrayexpress_id)
        ae_res = httr::content(httr::GET(request))
        protocol_ids = xml_text(xml_find_all(ae_res, "//protocol/accession"))
    }else{
        protocol_ids = NULL
    }
    #getting sample metadata
    sample_ids = parse_multiple_ids(sample_id)
    experiment_ids =  parse_multiple_ids(experiment_id)
    sample_info = c()
    if (length(sample_ids)>0){
        for (s in 1:length(sample_ids)) {
            request = paste0("https://www.ebi.ac.uk/ena/browser/api/xml/", sample_ids[s])
            sample_res = httr::content(httr::GET(request))
            organism_name = xml_text(xml_find_all(sample_res,"//SCIENTIFIC_NAME"))
            keywords = c(keywords, organism_name)
            
            
            sample_description = xml_text(xml_find_all(sample_res,"//SAMPLE/DESCRIPTION"))
            if (is.null(sample_description))
                sample_description = ""
            sample_atts =  xml_find_all(sample_res,"//SAMPLE/SAMPLE_ATTRIBUTES/SAMPLE_ATTRIBUTE")
            
            sample_characteristics = ""
            sample_characteristics = sapply(sample_atts, function(n){
                
                tag = xml_text(xml_find_all(n, "./TAG"))
                if (grepl("[A-Z]", tag) == FALSE){
                    value = xml_text(xml_find_all(n, "./VALUE"))
                    sample_characteristics = paste0(tag, " :", value, "; ")
                }else{
                    sample_characteristics = ""
                }
            })
            sample_characteristics = paste(sample_characteristics, collapse = "<br/>")
            sample_info = c(sample_info, paste("<h4>", organism_name,"</h4", "<br/>", sample_description, "<br/><br/>", sample_characteristics, "<br/>"))
        }
    }
    
    #getting experiment (methods) metadata
    experiment_info = c()
    library_strategies = c()
    sequencing_platforms = c()
    if (length(experiment_ids)>0){
        for (s in 1:length(experiment_ids)) {
            request = paste0("https://www.ebi.ac.uk/ena/browser/api/xml/", experiment_ids[s])
            ena_experiment = httr::content(httr::GET(request))
            library_strategy =  xml_text(xml_find_all(ena_experiment,"//EXPERIMENT/DESIGN/LIBRARY_DESCRIPTOR/LIBRARY_STRATEGY"))
            sequencing_platform = xml_text(xml_find_all(ena_experiment,"//EXPERIMENT/PLATFORM"))
            spot_count = xml_text(xml_find_all(ena_experiment,"//EXPERIMENT/EXPERIMENT_ATTRIBUTES/EXPERIMENT_ATTRIBUTE/VALUE[../TAG='ENA-SPOT-COUNT']"))
            base_count = xml_text(xml_find_all(ena_experiment,"//EXPERIMENT/EXPERIMENT_ATTRIBUTES/EXPERIMENT_ATTRIBUTE/VALUE[../TAG='ENA-BASE-COUNT']"))
            library_strategies = c(library_strategies, library_strategy)
            sequencing_platforms = c(sequencing_platforms, sequencing_platform)
        }
        sequencing_platforms = unique(sequencing_platforms)
        sequencing_platforms = paste(sequencing_platforms, collapse = "</br>")
        library_strategies = unique(library_strategies)
        keywords = c(keywords, sequencing_platforms)
        keywords = c(keywords, library_strategies)
    }
    protocol_ids = sort(protocol_ids)
    protocols = c()
    if (length(protocol_ids>0)){
        #requests for each protocol:
        protocols = sapply(protocol_ids, function(n){
            request = paste0("https://www.ebi.ac.uk/arrayexpress/xml/v3/protocols/", n)
            protocol_res = httr::content(httr::GET(request))
            protocol_title = xml_text(xml_find_all(protocol_res, "protocol/type"))
            protocol_title = paste("<h4>", protocol_title, "</h4>")
            protocol_text =  xml_text(xml_find_all(protocol_res, "protocol/text"))
            hardware = xml_text(xml_find_all(protocol_res, "protocol/hardware"))
            software = xml_text(xml_find_all(protocol_res, "protocol/software"))
            performer = xml_text(xml_find_all(protocol_res, "protocol/performer"))
            protocol = paste0(tools::toTitleCase(protocol_title), "<br/>", protocol_text, "<br/>")
            if (!(performer=="")){
                protocol = paste0(protocol, "Performed by: ", performer, "<br/>")
            }
            if (!(hardware=="")){
                protocol = paste0(protocol, "Hardware used: ", hardware, "<br/>")
            }
            if (!(software=="")){
                protocol = paste0(protocol, "Software used: ", software, "<br/>")
            }
            protocols = c(protocols, protocol)
        })
    }
    
    
    
    steps = paste(protocols, collapse = "<br/>")
    sample_info = unique(sample_info)
    sample_info = paste(sample_info, collapse = "<br/>")
    keywords = unique(keywords)
    keywords = paste(keywords, collapse = ", ")
    #display html formatted text
    display = c()
    display = paste(paste("<h1 style=\"color:TEAL;\">", title, "</h1>"), "<h2 style=\"color:DARKCYAN;\">Abstract</h2>", abstract, "<h2 style=\"color:DARKCYAN;\">Keywords</h2>", keywords, sep="<br/>")
    display = paste(display, "<h2 style=\"color:DARKCYAN;\">Introduction</h2>", " ", " ", "<h3 style=\"color:DARKCYAN;\">Value of the dataset</h3>", " ", "<h4 style=\"color:LIGHTSEAGREEN;\">Scientific value</h4>", "<h4 style=\"color:LIGHTSEAGREEN;\">Societal value</h4>", sep = "<br/>")
    display = paste(display, "<h2 style=\"color:DARKCYAN;\">Methods</h2>", steps, "<h3 style=\"color:DARKCYAN;\">Sampling</h3>", sample_info, "<h4 style=\"color:LIGHTSEAGREEN;\">Environmental profile </h4>", " ", "<h4 style=\"color:LIGHTSEAGREEN;\">Geographic range </h4>", " ", "<h4 style=\"color:LIGHTSEAGREEN;\">Technologies used</h4>",  sep = "<br/>")
    display = paste(display, "<h3 style=\"color:DARKCYAN;\">Sample processing</h3>", "<h4 style=\"color:LIGHTSEAGREEN;\">Technologies used</h4>", sequencing_platforms, sep = "<br/>")
    display = paste(display, "<h3 style=\"color:DARKCYAN;\">Data processing</h3>", " ", " ",  "<h4 style=\"color:LIGHTSEAGREEN;\">Technologies used</h4>", sep = "<br/>")
    display = paste(display, "<h2 style=\"color:DARKCYAN;\">Biodiversity profile</h2>", " ", "<h3 style=\"color:DARKCYAN;\">Target</h3>", library_strategies, "<h3 style=\"color:DARKCYAN;\">Taxonomic range</h3>", organism_name, "<h3 style=\"color:DARKCYAN;\">Functional range</h3>", " ", "<h3 style=\"color:DARKCYAN;\">Traits</h3>", " ", sep = "<br/>")
    display = paste(display, "<h2 style=\"color:DARKCYAN;\">Data Resources</h2>", dr,  sep = "<br/>")
    display = paste(display, "<h2 style=\"color:DARKCYAN;\">Data statistics</h2>", "", "<h2 style=\"color:DARKCYAN;\">Caveats and limitations</h2>", "", "<h2 style=\"color:DARKCYAN;\">Usage rights</h2>", sep = "<br/>")
    display = paste(display, "<h2 style=\"color:DARKCYAN;\">Supplementary material</h2>", " ", "<h3 style=\"color:DARKCYAN;\">Suppl. material 1: BioSamples MIxS checklists</h3>", " ", "<h4 style=\"color:LIGHTSEAGREEN;\">Data type: sample metadata</h4>", " ", "<h4 style=\"color:LIGHTSEAGREEN;\">Filename: biosamples_mixs_checklist.csv</h4>", " ")
    return(display)
}

parse_multiple_ids = function(id){
    if (length(id)>0){
        #check whether the separating character is - or ,
        #first check both
        if (grepl(",", id) && grepl("-", id)){
            #first split only based on ,
            ids = unlist(strsplit(id, ","))
            new_ids = c()
            new_ids = sapply(ids, function(i){
                if (grepl("-", i)){
                    new_ids = parse_consecutive_ids(i)
                }else{
                    new_ids = c(new_ids, i)
                }
            })
            names(new_ids) = NULL
            new_ids = unlist(new_ids)
        }else if (grepl("-", id)){
            new_ids = c()
            new_ids = sapply(id, function(i){
                if (grepl("-", i)){
                    new_ids = parse_consecutive_ids(i)
                }else{
                    new_ids = c(new_ids, i)
                }
            })
            names(new_ids) = NULL
            new_ids = unlist(new_ids)
        }else{
            new_ids = unlist(strsplit(id, ","))
        }
    }else{
        new_ids = id
    }
    return(new_ids)
}


parse_consecutive_ids = function(id){
    #e.g. DRX016751-DRX016758
    #split
    #extract number
    ids = unlist(strsplit(id, "-"))
    second_number = stringr::str_extract(ids[2], "[^a-zA-Z0][0-9]*?$")
    first_number =  stringr::str_extract(ids[1], "[^a-zA-Z0][0-9]*?$")
    diff = as.numeric(second_number)-as.numeric(first_number)
    letter_prefix = stringr::str_extract(ids[1], "^[a-zA-Z0]*")
    new_ids = c(ids[1])
    for (i in 1:diff){
        new_num = as.numeric(first_number)+i
        new_id = paste0(letter_prefix, new_num)
        new_ids = c(new_ids, new_id)
    }
    return(new_ids)
}

#generate the reactive shiny app 
ui = fluidPage(
    fluidRow(
        column(12,
               textInput(inputId = "id", label = "Enter ENA Study accession number", value =  "PRJDB2900", width = 600),
               htmlOutput(outputId  = "out"), padding = 2000
        )),
    fluidRow(
        column(12,
               DTOutput(outputId = "table"), padding = 2000
        )
    ))

server = function(input, output){
    display = reactive({
        process_study(input)
    })
    table = reactive({
        process_biosamples(input)
    })
    output$out <- renderUI({HTML(display())})
    output$table <-  DT::renderDataTable({table()})
    
    
}
shinyApp(ui=ui, server = server)

