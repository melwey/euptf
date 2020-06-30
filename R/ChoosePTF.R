ChoosePTF <-
function(){
# gWidget example: choice of European PTF
# Authors: M. Weynants and B. Tóth
# Date created: 2013/07/27
# Last modified: 2014/04/29
#############################################################
require(gWidgets)
require(gWidgetstcltk)
win <- gwindow("What PTF to use?",visible=FALSE)
big_grp <- ggroup(container = win)
data_grp <- ggroup(container = big_grp,horizontal=FALSE)
# label
lbl_data <- glabel(
	"Available data",
	container = data_grp
)
run_grp <- ggroup(container = big_grp)
output_grp <- ggroup(container = big_grp,horizontal=FALSE)# label
lbl_output <- glabel(
	"Output",
	container = output_grp
)
ths_grp <- ggroup(container = output_grp,horizontal=TRUE)
fc_grp <- ggroup(container = output_grp,horizontal=TRUE)
wp_grp <- ggroup(container = output_grp,horizontal=TRUE)
ks_grp <- ggroup(container = output_grp,horizontal=TRUE)
vg_grp <- ggroup(container = output_grp,horizontal=TRUE)
mv_grp <- ggroup(container = output_grp,horizontal=TRUE)

# Data group
# check boxes:
chk_topsoil <- gcheckbox(
  text      = "Topsoil/Subsoil",
  checked   = TRUE,
  container = data_grp
)
chk_fao <- gcheckbox(
  text      = "Modified FAO texture class (5 classes + organic)",
  checked   = FALSE,
  container = data_grp
)
chk_usda <- gcheckbox(
  text      = "USDA texture class (12 classes + organic)",
  checked   = FALSE,
  container = data_grp
)
chk_psd <- gcheckbox(
  text      = "Sand, silt, clay contents",
  checked   = FALSE,
  container = data_grp
)
chk_oc <- gcheckbox(
  text      = "Organic carbon content",
  checked   = FALSE,
  container = data_grp
)
chk_bd <- gcheckbox(
  text      = "Bulk density",
  checked   = FALSE,
  container = data_grp
)
chk_ph <- gcheckbox(
  text      = "pH in water",
  checked   = FALSE,
  container = data_grp
)
chk_cec <- gcheckbox(
  text      = "CEC",
  checked   = FALSE,
  container = data_grp
)
chk_caco3 <- gcheckbox(
  text      = "CaCO3",
  checked   = FALSE,
  container = data_grp
)


btn <- gbutton(
  text      = "Which PTF?",
  container = run_grp,
  handler   = function(h, ...)
  {
	topsoil <- svalue(chk_topsoil)
	fao <- svalue(chk_fao)
	usda <- svalue(chk_usda)
	psd <- svalue(chk_psd)
	oc <- svalue(chk_oc)
	bd <- svalue(chk_bd)
	ph <- svalue(chk_ph)
	cec <- svalue(chk_cec)
	caco3 <- svalue(chk_caco3)
	
	if (!topsoil){svalue(vg.result) <- "No PTF for these data. All PTFs need topsoil/subsoil information."
	  svalue(mv.result) <- "No PTF for these data. All PTFs need topsoil/subsoil information."
	  svalue(ths.result) <- "No PTF for these data. All PTFs need topsoil/subsoil information."
	  svalue(fc.result) <- "No PTF for these data. All PTFs need topsoil/subsoil information."
	  svalue(wp.result) <- "No PTF for these data. All PTFs need topsoil/subsoil information."
	  svalue(ks.result) <- "No PTF for these data. All PTFs need topsoil/subsoil information."}
	else {
	if (fao) {
		svalue(vg.result) <- "PTF18"#"VG_FAO_cPTF"
		svalue(mv.result) <- "PTF18"#"MV_FAO_cPTF"
		svalue(ths.result) <- "PTF01"#
		svalue(fc.result) <- "PTF07"#
		svalue(wp.result) <- "PTF10"#
		svalue(ks.result) <- "PTF13"#"KS_FAO_RT"
	}
	if (fao & oc) {
		svalue(ths.result) <- "PTF02"
		svalue(ks.result) <- "PTF14"#"KS_FAO_OC_RT"
	}
	if (usda) {
		svalue(vg.result) <- "PTF19"#"VG_USDA_cPTF"
		svalue(mv.result) <- "PTF19"#"MV_USDA_cPTF"
		svalue(ths.result) <- "PTF03"#
		svalue(fc.result) <- "PTF08"#
		svalue(wp.result) <- "PTF11"#
		svalue(ks.result) <- "PTF15"#"KS_USDA_RT"
	}
	if (psd) {
		svalue(vg.result) <- "PTF19"#"VG_USDA_cPTF"
		svalue(mv.result) <- "PTF19"#"MV_USDA_cPTF"
		svalue(ths.result) <- "PTF03"#
		svalue(fc.result) <- "PTF08"#
		svalue(wp.result) <- "PTF11"#
		svalue(ks.result) <- "PTF15"#"KS_USDA_RT"
	}
	# lucas_basic
	if (psd & oc) {
		svalue(ths.result) <- "PTF04"#"THS_LUCAS_BASIC_RT"
		svalue(fc.result) <- "PTF09"#"FC_LUCAS_BASIC_LR_t"
		svalue(wp.result) <- "PTF12"#"WP_LUCAS_BASIC_LR_t"
		svalue(ks.result) <- "PTF16"#"KS_LUCAS_BASIC_RT"
	}
	# hwsd_basic
	if (psd & oc & bd) {
		svalue(vg.result) <- "PTF21"#"VG_HWSD_BASIC_LR"
		svalue(ths.result) <- "PTF05"#"THS_HWSD_BASIC_LR_t"
	}
	# chem+
	if (psd & bd & ph){
		svalue(ths.result) <- "PTF06"#THS_GSM_LR_t
	}
	if (psd & ph & cec) {
		svalue(ks.result) <- "PTF17"#"KS_LUCAS_LR"
	}
	if (psd & oc & ph & cec) {
		svalue(vg.result) <- "PTF20"#"LUCAS_LR_VG"
	}
	if (psd & oc & bd & ph) {
		svalue(vg.result) <- "PTF22"#"VG_GSM_LR_t2"
	}
	}
	}
)

# Output group

# static text boxes:
ths <- glabel(
  text      = "Saturated water content\t\t\t\t",
  container = ths_grp
)
fc <- glabel(
  text      = "Water content at field capacity\t\t\t",
  container = fc_grp
)
wp <- glabel(
  text      = "Water content at wilting point\t\t\t",
  container = wp_grp
)
ks <- glabel(
  text      = "Saturated hydraulic conductivity\t\t\t",
  container = ks_grp
)
vg <- glabel(
  text      = "van Genuchten parameters\t\t\t\t",
  container = vg_grp
)
mv <- glabel(
  text      = "Mualem-van Genuchten parameters\t\t\t",
  container = mv_grp
)

# static text boxes updtaed by btn:
ths.result <- glabel(
  text      = "No PTF for these data. Try another combination.", 
  container = ths_grp,
  font.attr=list(style="bold")
)
fc.result <- glabel(
  text      = "No PTF for these data. Try another combination.", 
  container = fc_grp,
  font.attr=list(style="bold")
)
wp.result <- glabel(
  text      = "No PTF for these data. Try another combination.", 
  container = wp_grp,
  font.attr=list(style="bold")
)
ks.result <- glabel(
  text      = "No PTF for these data. Try another combination.", 
  container = ks_grp,
  font.attr=list(style="bold")
)
vg.result <- glabel(
  text      = "No PTF for these data. Try another combination.", 
  container = vg_grp,
  font.attr=list(style="bold")
)
mv.result <- glabel(
  text      = "No PTF for these data. Try another combination.", 
  container = mv_grp,
  font.attr=list(style="bold")
)
visible(win) <- TRUE
return(win)
}
