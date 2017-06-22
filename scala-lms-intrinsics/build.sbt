//      ____   __     ____   ____   ___   ____  ___ _____
//     / __ \ / /    / __ \ /  _/  |__ \ / __ \<  //__  /
//    / /_/ // /    / / / / / /    __/ // / / // /   / /
//   / ____// /___ / /_/ /_/ /    / __// /_/ // /   / /
//  /_/    /_____//_____//___/   /____/\____//_/   /_/
//
//  https://github.com/astojanov/lms-tutorial-pldi
//  Copyright (C) 2017 Alen Stojanov (astojanov@inf.ethz.ch)
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program. If not, see http://www.gnu.org/licenses/.
//

name := "LMS Tutorial - PLDI 2017"

version := "1.0"

scalaVersion := "2.11.2"

// LMS uses Scala Virtualized
scalaOrganization := "org.scala-lang.virtualized"

scalacOptions += "-Yvirtualize"

// LMS dependency
libraryDependencies += "org.scala-lang.lms" % "lms-core_2.11" % "0.9.0"

// Include the resolver to get the latest snapshots
resolvers += Resolver.sonatypeRepo("snapshots")

// support for unsigned primives in Scala
libraryDependencies += "ch.ethz.acl" %% "scala-unsigned" % "0.1-SNAPSHOT"

// the main lms-intrinsics package
libraryDependencies += "ch.ethz.acl" %% "lms-intrinsics" % "0.0.2-SNAPSHOT"

// package used for C pretty printing
libraryDependencies += "com.github.abrarsyed.jastyle" % "jAstyle" % "1.2"
