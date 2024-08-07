% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_skills.R
\name{dv_insert_sets}
\alias{dv_insert_sets}
\title{Insert setting actions for attacks that have not been scouted with sets}
\usage{
dv_insert_sets(
  x,
  no_set_attacks = c("PP", "P2", "PR"),
  phase_select = "Reception",
  default_set_evaluation = "+",
  attack_rows,
  set_call_table
)
}
\arguments{
\item{x}{datavolley: datavolley object as returned by \code{\link[datavolley:read_dv]{datavolley::dv_read()}}}

\item{no_set_attacks}{character: vector of attack codes for which we will not automatically insert sets (e.g. setter tips, overpass attacks)}

\item{phase_select}{character: play phase(s) of attacks to consider. One or more of "Reception", "Transition"}

\item{default_set_evaluation}{string: the default evaluation code for a set (used unless the attack was against 0 or 1 blockers, in which case it gets "#")}

\item{attack_rows}{integer: a vector of row numbers of attacks for which sets should be inserted. Automatically calculated if not provided}

\item{set_call_table}{data.frame: a data.frame with columns \code{attack_code} and \code{set_call}. Setter calls will be added to sets associated with attack codes in this list. Note that setter calls from this table will NOT be inserted on sets where the setter did not set the middle hitter (e.g. if the middle ran X1 but the setter set someone else, no "K1" call can be inserted because there is no way of knowing what the middle was running). This gives a biased set of setter call entries that are unlikely to be useful for analysis purposes. It is therefore recommended that you provide \code{set_call_table} to this function ONLY if you are then going to manually insert setter calls on the remaining rows}
}
\value{
A modified copy of \code{x}
}
\description{
This function will insert setting actions prior to attacks, where those attacks do not already have a set scouted. The sets are assigned to the setter on court for that team. It is therefore possible to scout a match, only manually scouting the setting actions where they were made by a player other than the designated setter. The remaining setting actions (made by the designated setter) can be filled in using this function, making the live scouting a little more efficient. Note, however, that automatically-inserted sets do not have the full information that can be included when scouting manually, including setter calls (but see the note on the \code{set_call_table} parameter) and location of the set.
}
\examples{
\dontrun{
  x <- dv_read(dv_example_file())
  sum(plays(x)$skill == "Set", na.rm = TRUE)
  x <- dv_insert_sets(x)
  sum(plays(x)$skill == "Set", na.rm = TRUE)
}

}
