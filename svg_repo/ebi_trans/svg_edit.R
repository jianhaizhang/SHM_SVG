svg.edit <- function(path.in, path.out) {

  library(XML); library(xml2)
  xmlfile <- xmlParse(path.in); xmltop <- xmlRoot(xmlfile)
  spa <- xmlGetAttr(xmltop, 'xml:space')
  # xml:space="preserve" results in emplty text nodes in the saved SVG image, so should be changed.
  if (!is.null(spa)) if (spa=='preserve') { 

    addAttributes(xmltop, 'xml:space'='none')
  
  }
  style <- 'fill:#62fafa;fill-opacity:1;stroke:#000000;stroke-width:0.21602426;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1'
  out <- xmltop[[xmlSize(xmltop)-1]]; ply <- xmltop[[xmlSize(xmltop)]]
  # Move base polygons to the top of the last group.
  if (xmlName(out)=='g') for (i in rev(seq_len(xmlSize(out)))) {

    if (xmlName(out[[i]])=='path'|xmlName(out[[i]])=='g') { addChildren(ply, kid=out[[i]], at=0) } 

  }; removeNodes(out) 

  # Change 'id' and 'style' of target polygons.
  ids <- NULL; for (i in seq_len(xmlSize(ply))) {

    if (is.null(ply[[i]][[1]])) addAttributes(ply[[i]], style=style)

    # Apply to original svg code downloaded from 'https://github.com/ebi-gene-expression-group/anatomogram/tree/master/src/svg'. Assign names to each path or group.
    if (any(names(ply[[i]])=='title')) { 
	  
      id <- make.names(xmlValue(ply[[i]][['title']]))
      ont <- xmlGetAttr(ply[[i]], 'id') 
      addAttributes(ply[[i]], id=id); addAttributes(ply[[i]], ontology=ont)
      names(id) <- ont; ids <- c(ids, id)
      removeNodes(ply[[i]][['title']]); addAttributes(ply[[i]], style=style)
  
    }

    if (xmlSize(ply[[i]])>=1) for (j in seq_len(xmlSize(ply[[i]]))) { addAttributes(ply[[i]][[j]], style=style) }

  }; for (i in seq_len(xmlSize(ply))) if (xmlName(ply[[i]])=='a') removeNodes(ply[[i]])

  # 'use' node outside 'g' group: Copy the referenced node and use it to replace the use node. Keep the 'id' of use node unchanged.
  w <- which(names(ply)=='use'); if (length(w)!=0) { 

    label <- NULL; for (i in seq_len(xmlSize(ply))) { label <- c(label, paste0('#', xmlGetAttr(ply[[i]], 'inkscape:label'))) }
    for (i in w) {
      
      # Keep attributes of 'use' node unchanged to avoid polygon shifting in position.
      nod <- ply[[i]]; att <- xmlAttrs(nod); na <- names(att); val <- as.character(att); names(val) <- na
      w1 <- which(label %in% xmlGetAttr(nod, 'xlink:href')); id <- xmlGetAttr(nod, 'id')
      ply[[i]] <- xmlClone(ply[[w1]], recursive=TRUE); addAttributes(ply[[i]], .attrs=val); addAttributes(ply[[i]], ontology=id)

    }

  }

  na <- strsplit(path.in, '/')[[1]]; na <- na[grep('.svg$', na)]; pa <- paste0(path.out, '/', na)
  # Name the container group.
  xmlAttrs(xmltop[[xmlSize(xmltop)]])[['id']] <- 'container'
  
  # Check 'use' node inside 'g' group.
  w <- which(names(ply)=='g'); for (i in w) {

    nod <- ply[[i]]; w1 <- which(names(nod)=='use')
    if (length(w1)!=0) { cat(xmlGetAttr(nod, 'id'), ': \'use\' inside \'group\'', '\n') }

  }
  saveXML(xmlfile, file=pa); cat(pa, '\n'); return(ids)

}

svg <- list.files(path='/home/jzhan067/SVG_tutorial_file/anatomogram/src/svg/', pattern='.svg$', full.names=T)
# In the svg tag, xml:space="preserve" introduces empty text nodes, e.g. mus_musculus.brain.svg. Thus this attribut should be removed.  
# Avoid running this function to all SVG images, since some images have dislocated polygons and need adjustment.
# for (i in svg[8]) svg.edit(path.in=i, path.out='/home/jzhan067/SVG_tutorial_file/anatomogram/src/svg/svg_trans')

# Name the container group.
library(XML)
svg1 <- list.files(path='/home/jzhan067/SVG_tutorial_file/svg_repo/ebi_trans', pattern='.svg$', full.names=T)
path.out <- '/home/jzhan067/SVG_tutorial_file/svg_repo/ebi_trans/'

for (i in svg1) {

  path.in <- i
  xmlfile <- xmlParse(path.in); xmltop <- xmlRoot(xmlfile)
  xmlAttrs(xmltop[[xmlSize(xmltop)]])[['id']] <- 'container'
  na <- strsplit(path.in, '/')[[1]]; na <- na[grep('.svg$', na)]; pa <- paste0(path.out, '/', na)
  saveXML(xmlfile, file=pa); cat(pa, '\n')

}

# Make names for ids.
for (path.in in svg1) {

  xmlfile <- xmlParse(path.in); xmltop <- xmlRoot(xmlfile)
  ply <- xmltop[[xmlSize(xmltop)]]
  for (j in seq_len(xmlSize(ply))) { 
    
    id0 <- make.names(xmlGetAttr(ply[[j]], 'id'))
    xmlAttrs(ply[[j]])[['id']] <- id0

  }
  na <- strsplit(path.in, '/')[[1]]; na <- na[grep('.svg$', na)]; pa <- paste0(path.out, '/', na)
  saveXML(xmlfile, file=pa); cat(pa, '\n')

}


# In original EBI SVG, some tissues have label/ontology id while others donot, so should not rely on label to add the ontology attribute. 
path.out <- '~/test/'; for (i in svg) svg.edit(path.in=i, path.out=path.out)

id.ont <- NULL; for (path.in in svg1) {

  na <- strsplit(path.in, '/')[[1]]; na <- na[grep('.svg$', na)]
  xmlfile <- xmlParse(path.in); xmltop <- xmlRoot(xmlfile)
  ply <- xmltop[[xmlSize(xmltop)]]
  path.in1 <- paste0(path.out, na) 
  xmlfile1 <- xmlParse(path.in1); xmltop1 <- xmlRoot(xmlfile1)
  ply1 <- xmltop1[[xmlSize(xmltop1)]]

  ids <- NULL; for (j in seq_len(xmlSize(ply))) {

    id <- xmlGetAttr(ply[[j]], 'id')
    # lab <- xmlGetAttr(ply[[j]], 'label')
    for (k in seq_len(xmlSize(ply1))) {

      id1 <- xmlGetAttr(ply1[[k]], 'id')
      # lab1 <- xmlGetAttr(ply1[[k]], 'label') 
      # if (!is.null(lab) & !is.null(lab1) & is.null(xmlGetAttr(ply[[j]], 'href'))) { if (lab==lab1) { 
        
       # ont <- xmlGetAttr(ply1[[k]], 'ontology')
       # addAttributes(ply[[j]], ontology=ont)
       # names(id) <- ont; ids <- c(ids, id); break

      # } } else { # If there are two "if" in front, they should be separated by "{}". Otherwise the "else" follows the 2nd "if".

        if (id==id1) {

          ont <- xmlGetAttr(ply1[[k]], 'ontology')
          addAttributes(ply[[j]], ontology=ont)
          names(id) <- ont; ids <- c(ids, id); break

        }

     # }

    }

  }; lis <- list(ids); names(lis) <- na; id.ont <- c(id.ont, lis)  
  saveXML(xmlfile, file=path.in); cat(path.in, '\n')

}

library(rols)

df <- NULL; for (i in seq_along(id.ont)) {

  if (is.null(id.ont[[i]])) next
  tissue <- id.ont[[i]]; tissue <- tissue[names(tissue)!='']
  df0 <- as.data.frame(tissue, stringsAsFactors=FALSE)
  df0$ontology <- rownames(df0); df0$SVG <- names(id.ont[i]) 
  df <- rbind(df, df0)

}; rownames(df) <- NULL

df$description <- NA
for (i in seq_len(nrow(df))) {

  ont <- df[i, 'ontology']; abbr <- tolower(sub('_.*', '', ont))
  trm <- tryCatch({ term(abbr, ont) }, error=function(e) { return(NA) })
  if (is(trm, 'Term')) { des <- termDesc(trm); if (!is.null(des)) df[i, 'description'] <- termDesc(trm) }
  if (!is(trm, 'Term')) next
  
}

species='homo sapiens'; tissue='parotid.gland'

retrunSVGFeature <- function(tissue, species) {

sp <- sub(' |_|\\.|-', '|', species); tis <- sub(' |_|\\.|-', '|', tissue)
w <- grepl(sp, df$SVG, ignore.case=TRUE) & grepl(tis, df$tissue, ignore.case=TRUE); return(df[w, ])

}

retrunSVGFeature(tissue='parotid.gland', species='homo sapiens')
