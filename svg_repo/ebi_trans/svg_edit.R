svg.edit <- function(path.in, path.out) {

  library(xml2)
  doc <- read_xml(path.in); spa <- xml_attr(doc, 'space')
  if (!is.na(spa)) if (spa=='preserve') xml_set_attr(doc, 'xml:space', 'default')

  len <- xml_length(doc)
  out <- xml_children(doc)[[len-1]]; ply <- xml_children(doc)[[len]]; xml_set_attr(ply, 'id', 'container')
  # Move outline polygons to the top of the last group.
  if (xml_name(out)=='g') for (i in rev(seq_len(xml_length(out)))) {

    na <- xml_name(xml_children(out)[[i]])
    if (na=='path'|na=='g') { xml_add_child(.x=ply, .value=xml_children(out)[[i]], .where=0, .copy=FALSE) } 

  }; xml_remove(out, free=FALSE)

  # Change 'id' and 'style' of target polygons.
  style <- 'fill:#62fafa;fill-opacity:1;stroke:#000000;stroke-width:0.21602426;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1'
  chdn <- xml_children(ply); xml_set_attr(chdn, 'style', style)
  ids <- NULL; for (i in seq_len(xml_length(ply))) {

    # Apply to original svg code downloaded from 'https://github.com/ebi-gene-expression-group/anatomogram/tree/master/src/svg'. Assign names to each path or group.
    chdn1 <- xml_children(chdn[[i]]); na <- xml_name(chdn1)
    if (any(na=='title')) { 
	 
      tit <- chdn1[na=='title']
      # Use title id as id.
      id <- make.names(xml_attr(tit, 'id'))
      # Use title text as id. id <- make.names(xml_text(tit))
      xml_remove(tit, free=FALSE) # Only nodes with a title are assigned an "ontology". 
      ont <- xml_attr(chdn[[i]], 'id')
      xml_set_attr(chdn[[i]], 'id', id); xml_set_attr(chdn[[i]], 'ontology', ont)      
      names(id) <- ont; ids <- c(ids, id)
 
    }

  }; na <- xml_name(chdn); xml_remove(chdn[na=='a'], free=FALSE)  

  # 'use' node outside 'g' group: copy the referenced node and use it to replace the use node. Keep the 'id' of use node unchanged.
  w <- which(xml_name(chdn)=='use'); if (length(w)!=0) { 

    label <- NULL; for (i in seq_len(xml_length(ply))) { label <- c(label, paste0('#', xml_attr(chdn[[i]], 'ontology'))) }
    for (i in w) {
      
      # Keep attributes of 'use' node unchanged to avoid polygon shifting in position.
      nod <- chdn[[i]]; att <- xml_attrs(nod)
      w1 <- which(label %in% xml_attr(nod, 'href'))
      xml_replace(chdn[[i]], .value=chdn[[w1]], .copy=TRUE)

    }

  }
 
  # Check 'use' node inside 'g' group.
  w <- which(xml_name(xml_children(ply))=='g'); for (i in w) {

    nod <- xml_children(ply)[[i]]
    w1 <- xml_name(xml_children(nod))=='use'
    if (any(w1)) { cat(xml_attr(nod, 'id'), ': \'use\' inside \'group\'', '\n') }

  }
  
  na <- strsplit(path.in, '/')[[1]]; na <- na[grep('.svg$', na)]; pa <- paste0(path.out, '/', na)
  write_xml(doc, file=pa); cat(pa, '\n'); return(ids)

}

svg <- list.files(path='/home/jzhan067/SVG_tutorial_file/anatomogram/src/svg/', pattern='.svg$', full.names=T)
# In the svg tag, xml:space="preserve" introduces empty text nodes, e.g. mus_musculus.brain.svg. Thus this attribut should be removed.  
# Avoid running this function to all SVG images, since some images have dislocated polygons and need adjustment.
# for (i in svg[8]) svg.edit(path.in=i, path.out=path.out)

# Name the container group.
library(xml2)
svg1 <- list.files(path='/home/jzhan067/SVG_tutorial_file/svg_repo/ebi_trans', pattern='.svg$', full.names=T)
path.out <- '/home/jzhan067/SVG_tutorial_file/svg_repo/ebi_trans/'
path.out <- '~/test/'

id.ont <- NULL; for (path.in in svg1) {

  doc <- read_xml(path.in); chdn <- xml_children(doc)
  ply <- chdn[[xml_length(doc)]]; chdn1 <- xml_children(ply)
  na <- strsplit(path.in, '/')[[1]]; na <- na[grep('.svg$', na)]
  path.in1 <- paste0(path.out, na) 
  doc1 <- read_xml(path.in1); chdn2 <- xml_children(doc1)
  ply1 <- chdn2[[xml_length(doc1)]]; chdn3 <- xml_children(ply1)  

  ids <- NULL; for (i in seq_along(chdn1)) {

    ont <- xml_attr(chdn1[[i]], 'ontology')
    if (is.na(ont)) next
    id <- xml_attr(chdn1[[i]], 'id')
    for (j in seq_along(chdn3)) {

      ont1 <- xml_attr(chdn3[[j]], 'ontology')
      if (is.na(ont1)) next
      id1 <- xml_attr(chdn3[[j]], 'id') 
      if (ont==ont1) { 

        xml_attr(chdn1[[i]], 'id') <- id1
        names(id1) <- ont; ids <- c(ids, id1); break
      
      }

    }

  }; lis <- list(ids); names(lis) <- na; id.ont <- c(id.ont, lis)  
  write_xml(doc, file=path.in); cat(path.in, '\n')

}


for (path.in in svg1) {

  doc <- read_xml(path.in); chdn <- xml_children(doc)
  xml_attr(chdn[[xml_length(doc)]], 'id') <- 'container' 
  na <- strsplit(path.in, '/')[[1]]; na <- na[grep('.svg$', na)]; pa <- paste0(path.out, '/', na)
  write_xml(doc, file=pa); cat(pa, '\n')

}

# Make names for ids.
library(XML)
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
