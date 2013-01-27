## Copyright 2012-2013 Sebastian Gibb
## <mail@sebastiangibb.de>
##
## This file is part of MALDIquant for R and related languages.
##
## MALDIquant is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## MALDIquant is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with MALDIquant. If not, see <http://www.gnu.org/licenses/>

## AbstractMassObject
setMethod("trim",
          signature=signature(object="AbstractMassObject", minMass="numeric",
                              maxMass="numeric"),
          definition=function(object, minMass, maxMass) {

  i <- minMass <= object@mass & object@mass <= maxMass

  return(object[i])
})

setMethod("trim",
          signature=signature(object="AbstractMassObject", minMass="missing",
                              maxMass="numeric"),
          definition=function(object, minMass, maxMass) {

  return(trim(object, minMass=min(object@mass, na.rm=TRUE), maxMass=maxMass))
})

setMethod("trim",
          signature=signature(object="AbstractMassObject", minMass="numeric",
                              maxMass="missing"),
          definition=function(object, minMass, maxMass) {

  return(trim(object, minMass=minMass, maxMass=max(object@mass, na.rm=TRUE)))
})

## list
setMethod("trim",
          signature=signature(object="list", minMass="numeric",
                              maxMass="numeric"),
          definition=function(object, minMass, maxMass) {
  return(lapply(object, trim, minMass=minMass, maxMass=maxMass))
})

## list
setMethod("trim",
          signature=signature(object="list", minMass="missing",
                              maxMass="numeric"),
          definition=function(object, minMass, maxMass) {
  return(lapply(object, trim, maxMass=maxMass))
})

## list
setMethod("trim",
          signature=signature(object="list", minMass="numeric",
                              maxMass="missing"),
          definition=function(object, minMass, maxMass) {
  return(lapply(object, trim, minMass=minMass))
})

## AbstractMassObject
setMethod("ltrim",
          signature=signature(object="AbstractMassObject", minMass="numeric"),
          definition=function(object, minMass) {
  return(trim(object, minMass=minMass))
})

## list
setMethod("ltrim",
          signature=signature(object="list", minMass="numeric"),
          definition=function(object, minMass) {
  return(lapply(object, trim, minMass=minMass))
})

## AbstractMassObject
setMethod("rtrim",
          signature=signature(object="AbstractMassObject", maxMass="numeric"),
          definition=function(object, maxMass) {
  return(trim(object, maxMass=maxMass))
})

## list
setMethod("rtrim",
          signature=signature(object="list", maxMass="numeric"),
          definition=function(object, maxMass) {
  return(lapply(object, trim, maxMass=maxMass))
})

