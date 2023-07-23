phylopicIDs <- c("Tinamiformes"="14ddbf4d-7749-4153-bbe1-8d0c6ffdf142",
                 "Galliformes"="e1681c65-e946-40c6-9199-ac816b1c0040",
                 "Anseriformes"="3ceaa22b-8879-4545-9e32-425010f33cd4",
                 "Caprimulgiformes"="b4f64736-abc3-429d-b19e-4c7f74b291c4",
                 "Pterocliformes"="480610c8-7a3d-4ffe-adbd-95e8a68440c4",
                 "Columbiformes"="966eed5e-b869-41b4-8de7-32286ae3656a",
                 "Phoenicopteriformes"="a1244226-f2c2-41dc-b113-f1c6545958ce",
                 "Podicipediformes"="926d7e08-a609-4321-9791-8d52c77702c0",
                 "Otidiformes"="1b5a326b-40b8-4434-b5ea-881dc2a0e654",
                 "Cuculiformes"="5311f33f-6db1-47b0-94c4-c60fa7521884",
                 "Gruiformes"="cd5ca1a2-b0a9-4ce9-b163-5c004e327d9d",
                 "Gaviiformes"="db5c0243-52bf-4a2b-a27c-0a0c34469522",
                 "Ciconiiformes"="8691fc60-e7f2-4a69-af65-08461defac6b",
                 "Suliformes"="21a3420c-8d7a-4bd2-8ff7-b89fe7f5add5",
                 "Pelecaniformes"="9b0855e1-4ca1-44e4-939a-d8d20c80cbce",
                 "Procellariiformes"="e7e96370-52a7-4760-9c08-bb7ff6a0ea6d",
                 "Charadriiformes"="65ed3e2d-38f5-421f-976c-be4df6ac73fa",
                 "Cathartiformes"="73528af2-c4ca-4bc4-81ca-d22126220fa9",
                 "Accipitriformes"="93e8bd48-f8e0-457a-8551-670f2684b7f0",
                 "Strigiformes"="ea71ef37-b1ce-428b-84d0-438d0d954f32",
                 "Bucerotiformes"="6c6ccca8-64e1-4cf0-914b-992ba77de847",
                 "Coraciiformes"="58b1d404-24a6-454b-bf72-2fa5de885a1e",
                 "Piciformes"="1ae252c6-c345-4174-8aef-72f134ddea2d",
                 "Falconiformes"="1d5f26b6-4a9a-48ad-b799-a83eadb078ca",
                 "Psittaciformes"="0d19238f-25ca-42a5-a1e2-d108defb75f1",
                 "Passeriformes"="3a4cdd72-e553-40ad-838e-3b23037b2010")

picsToKeep <- c("Tinamiformes",
                "Galliformes", 
                "Anseriformes", 
                "Columbiformes", 
                "Gruiformes", 
                "Suliformes",
                "Procellariiformes",
                "Caprimulgiformes",
                "Charadriiformes",
                "Coraciiformes",
                "Strigiformes",
                "Accipitriformes",
                "Falconiformes",
                "Psittaciformes",
                "Passeriformes")


sizes <- c("Tinamiformes" = 0.10,
           "Galliformes" = 0.08,
           "Anseriformes" = 0.08,
           "Columbiformes" = 0.08,
           "Gruiformes" = 0.06,
           "Suliformes" = 0.08, 
           "Procellariiformes" = 0.05,
           "Caprimulgiformes" = 0.05,
           "Charadriiformes" = 0.09,
           "Coraciiformes" = 0.08,
           "Strigiformes" = 0.08,
           "Accipitriformes" = 0.06,
           "Falconiformes" = 0.06,
           "Psittaciformes" = 0.05,
           "Passeriformes" = 0.08)

getOrderParent <- function(order, data, tree) {
    orderTips <- filter(data, Order_Clements==order & (Label_BirdTree %in% tree$tip.label))$Label_BirdTree
    parentNode <- NA
    if (length(orderTips) > 1) {
        parentNode <- getMRCA(tree, orderTips)  
    }
    return(parentNode)
}

getOrderLabels <- function(gtree, data, tree) {
    orderLabels <- tibble(Order_Label=unique(gtree$data$Order_Clements)) |>
                filter(!is.na(Order_Label)) |>
                filter(Order_Label %in% picsToKeep) |>
                mutate(Parent_Node=map_dbl(Order_Label, ~getOrderParent(., data, tree))) |>
                filter(!is.na(Parent_Node)) |>
                mutate(Phylo_ID = phylopicIDs[Order_Label],
                       Image_Size = sizes[Order_Label],
                       Blank="")
    return(orderLabels)
}
