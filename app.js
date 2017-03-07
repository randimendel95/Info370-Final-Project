strict;

function getUserInput() {
    var sector = document.getElementById("sector").value;
    var fin_indep = document.getElementById("fin_indep").value;
    var pre_degree = document.getElementById("pre_degree").value;
    var firstgen_pct = document.getElementById("firstgen_pct").value;
    var fam_income = document.getElementById("fam_income").value;

    alert(sector + " " + fin_indep + " " + pre_degree + " " + firstgen_pct + " " + fam_income);
}