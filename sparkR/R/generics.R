#
# Licensed to the Apache Software Foundation (ASF) under one or more
# contributor license agreements.  See the NOTICE file distributed with
# this work for additional information regarding copyright ownership.
# The ASF licenses this file to You under the Apache License, Version 2.0
# (the "License"); you may not use this file except in compliance with
# the License.  You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

# Convert R S3 to Generic

#' @exportMethod head
setGeneric('head')

############ RDD Actions and Transformations ############

# @rdname aggregateRDD
#' @seealso reduce
#' @exportMethod aggregateRDD
setGeneric("aggregateRDD", function(x, zeroValue, seqOp, combOp) { standardGeneric("aggregateRDD") })

# @rdname cache-methods
#' @exportMethod cache
setGeneric("cache", function(x) { standardGeneric("cache") })

# @rdname coalesce
#' @seealso repartition
#' @exportMethod coalesce
setGeneric("coalesce", function(x, numPartitions, ...) { standardGeneric("coalesce") })

# @rdname checkpoint-methods
#' @exportMethod checkpoint
setGeneric("checkpoint", function(x) { standardGeneric("checkpoint") })

# @rdname collect-methods
#' @exportMethod collect
setGeneric("collect", function(x, ...) { standardGeneric("collect") })

# @rdname collect-methods
#' @exportMethod collectAsMap
setGeneric("collectAsMap", function(x) { standardGeneric("collectAsMap") })

# @rdname collect-methods
#' @exportMethod collectPartition
setGeneric("collectPartition",
           function(x, partitionId) {
             standardGeneric("collectPartition")
           })

# @rdname count
#' @exportMethod count
setGeneric("count", function(x) { standardGeneric("count") })

# @rdname countByValue
#' @exportMethod countByValue
setGeneric("countByValue", function(x) { standardGeneric("countByValue") })

# @rdname distinct
#' @exportMethod distinct
setGeneric("distinct", function(x, numPartitions = 1) { standardGeneric("distinct") })

# @rdname filterRDD
#' @exportMethod filterRDD
setGeneric("filterRDD", function(x, f) { standardGeneric("filterRDD") })

# @rdname first
#' @exportMethod first
setGeneric("first", function(x) { standardGeneric("first") })

# @rdname flatMap
#' @exportMethod flatMap
setGeneric("flatMap", function(X, FUN) { standardGeneric("flatMap") })

# @rdname fold
#' @seealso reduce
#' @exportMethod fold
setGeneric("fold", function(x, zeroValue, op) { standardGeneric("fold") })

# @rdname foreach
#' @exportMethod foreach
setGeneric("foreach", function(x, func) { standardGeneric("foreach") })

# @rdname foreach
#' @exportMethod foreachPartition
setGeneric("foreachPartition", function(x, func) { standardGeneric("foreachPartition") })

#' The jrdd accessor function.
#' @exportMethod getJRDD
setGeneric("getJRDD", function(rdd, ...) { standardGeneric("getJRDD") })

# @rdname glom
#' @exportMethod glom
setGeneric("glom", function(x) { standardGeneric("glom") })

# @rdname keyBy
#' @exportMethod keyBy
setGeneric("keyBy", function(x, func) { standardGeneric("keyBy") })

#' @exportMethod lapply
setGeneric('lapply')

# @rdname lapplyPartition
#' @exportMethod lapplyPartition
setGeneric("lapplyPartition", function(X, FUN) { standardGeneric("lapplyPartition") })

# @rdname lapplyPartitionsWithIndex
#' @exportMethod lapplyPartitionsWithIndex
setGeneric("lapplyPartitionsWithIndex",
           function(X, FUN) {
             standardGeneric("lapplyPartitionsWithIndex")
           })

# @rdname lapply
#' @exportMethod map
setGeneric("map", function(X, FUN) { standardGeneric("map") })

# @rdname lapplyPartition
#' @exportMethod mapPartitions
setGeneric("mapPartitions", function(X, FUN) { standardGeneric("mapPartitions") })

# @rdname lapplyPartitionsWithIndex
#' @exportMethod mapPartitionsWithIndex
setGeneric("mapPartitionsWithIndex",
           function(X, FUN) { standardGeneric("mapPartitionsWithIndex") })

# @rdname maximum
#' @exportMethod maximum
setGeneric("maximum", function(x) { standardGeneric("maximum") })

# @rdname minimum
#' @exportMethod minimum
setGeneric("minimum", function(x) { standardGeneric("minimum") })

# @rdname sumRDD
#' @exportMethod sumRDD
setGeneric("sumRDD", function(x) { standardGeneric("sumRDD") })

# @rdname name
#' @exportMethod name
setGeneric("name", function(x) { standardGeneric("name") })

# @rdname numPartitions
#' @exportMethod numPartitions
setGeneric("numPartitions", function(x) { standardGeneric("numPartitions") })

# @rdname persist
#' @exportMethod persist
setGeneric("persist", function(x, newLevel) { standardGeneric("persist") })

# @rdname pipeRDD
#' @exportMethod pipeRDD
setGeneric("pipeRDD", function(x, command, env = list()) { standardGeneric("pipeRDD")})

# @rdname reduce
#' @exportMethod reduce
setGeneric("reduce", function(x, func) { standardGeneric("reduce") })

# @rdname repartition
#' @seealso coalesce
#' @exportMethod repartition
setGeneric("repartition", function(x, numPartitions) { standardGeneric("repartition") })

# @rdname sampleRDD
#' @exportMethod sampleRDD
setGeneric("sampleRDD",
           function(x, withReplacement, fraction, seed) {
             standardGeneric("sampleRDD")
           })

# @rdname saveAsObjectFile
#' @seealso objectFile
#' @exportMethod saveAsObjectFile
setGeneric("saveAsObjectFile", function(x, path) { standardGeneric("saveAsObjectFile") })

# @rdname saveAsTextFile
#' @exportMethod saveAsTextFile
setGeneric("saveAsTextFile", function(x, path) { standardGeneric("saveAsTextFile") })

# @rdname setName
#' @exportMethod setName
setGeneric("setName", function(x, name) { standardGeneric("setName") })

# @rdname sortBy
#' @exportMethod sortBy
setGeneric("sortBy",
           function(x, func, ascending = TRUE, numPartitions = 1) {
             standardGeneric("sortBy")
           })

# @rdname take
#' @exportMethod take
setGeneric("take", function(x, num) { standardGeneric("take") })

# @rdname takeOrdered
#' @exportMethod takeOrdered
setGeneric("takeOrdered", function(x, num) { standardGeneric("takeOrdered") })

# @rdname takeSample
#' @exportMethod takeSample
setGeneric("takeSample",
           function(x, withReplacement, num, seed) {
             standardGeneric("takeSample")
           })

# @rdname top
#' @exportMethod top
setGeneric("top", function(x, num) { standardGeneric("top") })

# @rdname unionRDD
#' @exportMethod unionRDD
setGeneric("unionRDD", function(x, y) { standardGeneric("unionRDD") })

# @rdname unpersist-methods
#' @exportMethod unpersist
setGeneric("unpersist", function(x, ...) { standardGeneric("unpersist") })

# @rdname zipRDD
#' @exportMethod zipRDD
setGeneric("zipRDD", function(x, other) { standardGeneric("zipRDD") })

# @rdname zipRDD
#' @exportMethod zipPartitions
setGeneric("zipPartitions", function(..., func) { standardGeneric("zipPartitions") },
           signature = "...")

# @rdname zipWithIndex
#' @seealso zipWithUniqueId
#' @exportMethod zipWithIndex
setGeneric("zipWithIndex", function(x) { standardGeneric("zipWithIndex") })

# @rdname zipWithUniqueId
#' @seealso zipWithIndex
#' @exportMethod zipWithUniqueId
setGeneric("zipWithUniqueId", function(x) { standardGeneric("zipWithUniqueId") })


############ Binary Functions #############

# @rdname cartesian
#' @exportMethod cartesian
setGeneric("cartesian", function(x, other) { standardGeneric("cartesian") })

# @rdname countByKey
#' @exportMethod countByKey
setGeneric("countByKey", function(x) { standardGeneric("countByKey") })

# @rdname flatMapValues
#' @exportMethod flatMapValues
setGeneric("flatMapValues", function(X, FUN) { standardGeneric("flatMapValues") })

# @rdname intersection
#' @exportMethod intersection
setGeneric("intersection", function(x, other, numPartitions = 1) {
  standardGeneric("intersection") })

# @rdname keys
#' @exportMethod keys
setGeneric("keys", function(x) { standardGeneric("keys") })

# @rdname lookup
#' @exportMethod lookup
setGeneric("lookup", function(x, key) { standardGeneric("lookup") })

# @rdname mapValues
#' @exportMethod mapValues
setGeneric("mapValues", function(X, FUN) { standardGeneric("mapValues") })

# @rdname sampleByKey
#' @exportMethod sampleByKey
setGeneric("sampleByKey",
           function(x, withReplacement, fractions, seed) {
             standardGeneric("sampleByKey")
           })

# @rdname values
#' @exportMethod values
setGeneric("values", function(x) { standardGeneric("values") })


############ Shuffle Functions ############

# @rdname aggregateByKey
#' @seealso foldByKey, combineByKey
#' @exportMethod aggregateByKey
setGeneric("aggregateByKey",
           function(x, zeroValue, seqOp, combOp, numPartitions) {
             standardGeneric("aggregateByKey")
           })

# @rdname cogroup
#' @exportMethod cogroup
setGeneric("cogroup",
           function(x, ..., numPartitions = 2L) {
             standardGeneric("cogroup")
           })

# @rdname combineByKey
#' @seealso groupByKey, reduceByKey
#' @exportMethod combineByKey
setGeneric("combineByKey",
           function(x, createCombiner, mergeValue, mergeCombiners, numPartitions) {
             standardGeneric("combineByKey")
           })

# @rdname foldByKey
#' @seealso aggregateByKey, combineByKey
#' @exportMethod foldByKey
setGeneric("foldByKey",
           function(x, zeroValue, func, numPartitions) {
             standardGeneric("foldByKey")
           })

# @rdname join-methods
#' @exportMethod fullOuterJoin
setGeneric("fullOuterJoin", function(x, y, numPartitions) { standardGeneric("fullOuterJoin") })

# @rdname groupByKey
#' @seealso reduceByKey
#' @exportMethod groupByKey
setGeneric("groupByKey", function(x, numPartitions) { standardGeneric("groupByKey") })

# @rdname join-methods
#' @exportMethod join
setGeneric("join", function(x, y, ...) { standardGeneric("join") })

# @rdname join-methods
#' @exportMethod leftOuterJoin
setGeneric("leftOuterJoin", function(x, y, numPartitions) { standardGeneric("leftOuterJoin") })

# @rdname partitionBy
#' @exportMethod partitionBy
setGeneric("partitionBy", function(x, numPartitions, ...) { standardGeneric("partitionBy") })

# @rdname reduceByKey
#' @seealso groupByKey
#' @exportMethod reduceByKey
setGeneric("reduceByKey", function(x, combineFunc, numPartitions) { standardGeneric("reduceByKey")})

# @rdname reduceByKeyLocally
#' @seealso reduceByKey
#' @exportMethod reduceByKeyLocally
setGeneric("reduceByKeyLocally",
           function(x, combineFunc) {
             standardGeneric("reduceByKeyLocally")
           })

# @rdname join-methods
#' @exportMethod rightOuterJoin
setGeneric("rightOuterJoin", function(x, y, numPartitions) { standardGeneric("rightOuterJoin") })

# @rdname sortByKey
#' @exportMethod sortByKey
setGeneric("sortByKey",
           function(x, ascending = TRUE, numPartitions = 1) {
             standardGeneric("sortByKey")
           })

# @rdname subtract
#' @exportMethod subtract
setGeneric("subtract",
           function(x, other, numPartitions = 1) {
             standardGeneric("subtract")
           })

# @rdname subtractByKey
#' @exportMethod subtractByKey
setGeneric("subtractByKey",
           function(x, other, numPartitions = 1) {
             standardGeneric("subtractByKey")
           })


################### Broadcast Variable Methods #################

# @rdname broadcast
#' @exportMethod value
setGeneric("value", function(bcast) { standardGeneric("value") })



####################  DataFrame Methods ########################

#' @rdname agg
#' @exportMethod agg
setGeneric("agg", function (x, ...) { standardGeneric("agg") })

#' @rdname arrange
#' @exportMethod arrange
setGeneric("arrange", function(x, col, ...) { standardGeneric("arrange") })

#' @rdname schema
#' @exportMethod columns
setGeneric("columns", function(x) {standardGeneric("columns") })

#' @rdname describe
#' @exportMethod describe
setGeneric("describe", function(x, col, ...) { standardGeneric("describe") })

#' @rdname nafunctions
#' @exportMethod dropna
setGeneric("dropna",
           function(x, how = c("any", "all"), minNonNulls = NULL, cols = NULL) {
             standardGeneric("dropna")
           })

#' @rdname nafunctions
#' @exportMethod na.omit
setGeneric('na.omit')
# setGeneric("na.omit",
#           function(x, how = c("any", "all"), minNonNulls = NULL, cols = NULL) {
#             standardGeneric("na.omit")
#           })

#' @rdname schema
#' @exportMethod dtypes
setGeneric("dtypes", function(x) { standardGeneric("dtypes") })

#' @rdname explain
#' @exportMethod explain
setGeneric("explain", function(x, ...) { standardGeneric("explain") })

#' @rdname except
#' @exportMethod except
setGeneric("except", function(x, y) { standardGeneric("except") })

#' @rdname nafunctions
#' @exportMethod fillna
setGeneric("fillna", function(x, value, cols = NULL) { standardGeneric("fillna") })

# We dont export it as stats::filter , use where instead
# @rdname filter
# @exportMethod filter
setGeneric("filter", function(x, condition) { standardGeneric("filter") })

#' @rdname groupBy
#' @exportMethod group_by
setGeneric("group_by", function(x, ...) { standardGeneric("group_by") })

#' @rdname DataFrame
#' @exportMethod groupBy
setGeneric("groupBy", function(x, ...) { standardGeneric("groupBy") })

#' @rdname insertInto
#' @exportMethod insertInto
setGeneric("insertInto", function(x, tableName, ...) { standardGeneric("insertInto") })

#' @rdname intersect
#' @exportMethod intersect
setGeneric('intersect')
#setGeneric("intersect", function(x, y) { standardGeneric("intersect") })

#' @rdname isLocal
#' @exportMethod isLocal
setGeneric("isLocal", function(x) { standardGeneric("isLocal") })

#' @rdname limit
#' @exportMethod limit
setGeneric("limit", function(x, num) {standardGeneric("limit") })

#' @rdname withColumn
#' @exportMethod mutate
setGeneric("mutate", function(x, ...) {standardGeneric("mutate") })

#' @rdname arrange
#' @exportMethod orderBy
setGeneric("orderBy", function(x, col) { standardGeneric("orderBy") })

#' @rdname printSchema
#' @exportMethod printSchema
setGeneric("printSchema", function(x) { standardGeneric("printSchema") })

#' @rdname withColumnRenamed
#' @exportMethod rename
setGeneric("rename", function(x, ...) { standardGeneric("rename") })

#' @rdname registerTempTable
#' @exportMethod registerTempTable
setGeneric("registerTempTable", function(x, tableName) { standardGeneric("registerTempTable") })

# dont export, use sample_frac instead
# @rdname sample
# @exportMethod sample
setGeneric("sample", function(x, withReplacement, fraction, seed) { standardGeneric("sample") })

#' @exportMethod Filter
setGeneric('Filter')

#' @rdname sample
#' @exportMethod sample_frac
setGeneric("sample_frac",
           function(x, withReplacement, fraction, seed) {
             standardGeneric("sample_frac")
           })

#' @rdname saveAsParquetFile
#' @exportMethod saveAsParquetFile
setGeneric("saveAsParquetFile", function(x, path) { standardGeneric("saveAsParquetFile") })

#' @rdname saveAsTable
#' @exportMethod saveAsTable
setGeneric("saveAsTable", function(df, tableName, source, mode, ...) {
  standardGeneric("saveAsTable")
})

#' @rdname write.df
#' @exportMethod write.df
setGeneric("write.df", function(df, path, ...) { standardGeneric("write.df") })

#' @rdname write.df
#' @exportMethod saveDF
setGeneric("saveDF", function(df, path, ...) { standardGeneric("saveDF") })

#' @rdname schema
#' @exportMethod schema
setGeneric("schema", function(x) { standardGeneric("schema") })

#' @rdname select
#' @exportMethod select
setGeneric("select", function(x, col, ...) { standardGeneric("select") } )

#' @rdname select
#' @exportMethod selectExpr
setGeneric("selectExpr", function(x, expr, ...) { standardGeneric("selectExpr") })

#' @rdname showDF
#' @exportMethod showDF
setGeneric("showDF", function(x,...) { standardGeneric("showDF") })

#' @rdname agg
#' @exportMethod summarize
setGeneric("summarize", function(x,...) { standardGeneric("summarize") })

# @rdname tojson
# @exportMethod toJSON
setGeneric("toJSON", function(x) { standardGeneric("toJSON") })

#' @rdname DataFrame
#' @exportMethod toRDD
setGeneric("toRDD", function(x) { standardGeneric("toRDD") })

#' @rdname unionAll
#' @exportMethod unionAll
setGeneric("unionAll", function(x, y) { standardGeneric("unionAll") })

#' @rdname filter
#' @exportMethod where
setGeneric("where", function(x, condition) { standardGeneric("where") })

#' @rdname withColumn
#' @exportMethod withColumn
setGeneric("withColumn", function(x, colName, col) { standardGeneric("withColumn") })

#' @rdname withColumnRenamed
#' @exportMethod withColumnRenamed
setGeneric("withColumnRenamed", function(x, existingCol, newCol) {
  standardGeneric("withColumnRenamed") })


###################### Column Methods ##########################

#' @exportMethod mean
setGeneric('mean')

#' @exportMethod substr
setGeneric('substr')

#' @exportMethod %in%
setGeneric('%in%')

#' @rdname column
#' @exportMethod approxCountDistinct
setGeneric("approxCountDistinct", function(x, ...) { standardGeneric("approxCountDistinct") })

#' @rdname column
#' @exportMethod asc
setGeneric("asc", function(x) { standardGeneric("asc") })

#' @rdname column
#' @exportMethod avg
setGeneric("avg", function(x, ...) { standardGeneric("avg") })

#' @rdname column
#' @exportMethod cast
setGeneric("cast", function(x, dataType) { standardGeneric("cast") })

#' @rdname column
#' @exportMethod cbrt
setGeneric("cbrt", function(x) { standardGeneric("cbrt") })

#' @rdname column
#' @exportMethod contains
setGeneric("contains", function(x, ...) { standardGeneric("contains") })

#' @rdname column
#' @exportMethod countDistinct
setGeneric("countDistinct", function(x, ...) { standardGeneric("countDistinct") })

#' @rdname column
#' @exportMethod desc
setGeneric("desc", function(x) { standardGeneric("desc") })

#' @rdname column
#' @exportMethod endsWith
setGeneric("endsWith", function(x, ...) { standardGeneric("endsWith") })

#' @rdname column
#' @exportMethod getField
setGeneric("getField", function(x, ...) { standardGeneric("getField") })

#' @rdname column
#' @exportMethod getItem
setGeneric("getItem", function(x, ...) { standardGeneric("getItem") })

#' @rdname column
#' @exportMethod hypot
setGeneric("hypot", function(y, x) { standardGeneric("hypot") })

#' @rdname column
#' @exportMethod isNull
setGeneric("isNull", function(x) { standardGeneric("isNull") })

#' @rdname column
#' @exportMethod isNotNull
setGeneric("isNotNull", function(x) { standardGeneric("isNotNull") })

#' @rdname column
#' @exportMethod last
setGeneric("last", function(x) { standardGeneric("last") })

#' @rdname column
#' @exportMethod like
setGeneric("like", function(x, ...) { standardGeneric("like") })

#' @rdname column
#' @exportMethod lower
setGeneric("lower", function(x) { standardGeneric("lower") })

#' @rdname column
#' @exportMethod n
setGeneric("n", function(x) { standardGeneric("n") })

#' @rdname column
#' @exportMethod n_distinct
setGeneric("n_distinct", function(x, ...) { standardGeneric("n_distinct") })

#' @rdname column
#' @exportMethod rint
setGeneric("rint", function(x, ...) { standardGeneric("rint") })

#' @rdname column
#' @exportMethod rlike
setGeneric("rlike", function(x, ...) { standardGeneric("rlike") })

#' @rdname column
#' @exportMethod startsWith
setGeneric("startsWith", function(x, ...) { standardGeneric("startsWith") })

#' @rdname column
#' @exportMethod sumDistinct
setGeneric("sumDistinct", function(x) { standardGeneric("sumDistinct") })

#' @rdname column
#' @exportMethod toDegrees
setGeneric("toDegrees", function(x) { standardGeneric("toDegrees") })

#' @rdname column
#' @exportMethod toRadians
setGeneric("toRadians", function(x) { standardGeneric("toRadians") })

#' @rdname column
#' @exportMethod upper
setGeneric("upper", function(x) { standardGeneric("upper") })
