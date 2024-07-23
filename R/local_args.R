
#' @title Local process launcher class that passes arguments to R sessions.
#'
#' @import crew
#' @export
#' @family local_args
#' @description `R6` class to launch and manage local process workers in R sessions that accept arguments.
#' @details See [crewargs_launcher_local_args()].
#' @param cmdargs (`character()`)\cr
#'   A character vector of command line arguments for the R sessions used by workers.
#' @param name Name of the launcher.
#' @param seconds_interval Number of seconds between
#'   polling intervals waiting for certain internal
#'   synchronous operations to complete,
#'   such as checking `mirai::status()`.
#' @param seconds_timeout Number of seconds until timing
#'   out while waiting for certain synchronous operations to complete,
#'   such as checking `mirai::status()`.
#' @param seconds_launch Seconds of startup time to allow.
#'   A worker is unconditionally assumed to be alive
#'   from the moment of its launch until `seconds_launch` seconds later.
#'   After `seconds_launch` seconds, the worker is only
#'   considered alive if it is actively connected to its assign websocket.
#' @param seconds_idle Maximum number of seconds that a worker can idle
#'   since the completion of the last task. If exceeded, the worker exits.
#'   But the timer does not launch until `tasks_timers` tasks
#'   have completed.
#'   See the `idletime` argument of `mirai::daemon()`. `crew` does not
#'   excel with perfectly transient workers because it does not micromanage
#'   the assignment of tasks to workers, so please allow enough idle
#'   time for a new worker to be delegated a new task.
#' @param seconds_wall Soft wall time in seconds.
#'   The timer does not launch until `tasks_timers` tasks
#'   have completed.
#'   See the `walltime` argument of `mirai::daemon()`.
#' @param seconds_exit Deprecated on 2023-09-21 in version 0.5.0.9002.
#'   No longer necessary.
#' @param tasks_max Maximum number of tasks that a worker will do before
#'   exiting. See the `maxtasks` argument of `mirai::daemon()`.
#'   `crew` does not
#'   excel with perfectly transient workers because it does not micromanage
#'   the assignment of tasks to workers, it is recommended to set
#'   `tasks_max` to a value greater than 1.
#' @param tasks_timers Number of tasks to do before activating
#'   the timers for `seconds_idle` and `seconds_wall`.
#'   See the `timerstart` argument of `mirai::daemon()`.
#' @param reset_globals `TRUE` to reset global environment
#'   variables between tasks, `FALSE` to leave them alone.
#' @param reset_packages `TRUE` to unload any packages loaded during
#'   a task (runs between each task), `FALSE` to leave packages alone.
#' @param reset_options `TRUE` to reset global options to their original
#'   state between each task, `FALSE` otherwise. It is recommended to
#'   only set `reset_options = TRUE` if `reset_packages` is also `TRUE`
#'   because packages sometimes rely on options they set at loading time.
#' @param garbage_collection `TRUE` to run garbage collection between
#'   tasks, `FALSE` to skip.
#' @param launch_max Positive integer of length 1, maximum allowed
#'   consecutive launch attempts which do not complete any tasks.
#'   Enforced on a worker-by-worker basis.
#'   The futile launch count resets to back 0
#'   for each worker that completes a task.
#'   It is recommended to set `launch_max` above 0
#'   because sometimes workers are unproductive under perfectly ordinary
#'   circumstances. But `launch_max` should still be small enough
#'   to detect errors in the underlying platform.
#' @param processes `NULL` or positive integer of length 1,
#'   number of local processes to
#'   launch to allow worker launches to happen asynchronously. If `NULL`,
#'   then no local processes are launched. If 1 or greater, then the launcher
#'   starts the processes on `start()` and ends them on `terminate()`.
#'   Plugins that may use these processes should run asynchronous calls
#'   using `launcher$async$eval()` and expect a `mirai` task object
#'   as the return value.

crewargs_class_launcher_local_args <- R6::R6Class(
  classname = "crewargs_class_launcher_local_args",
  inherit = crew::crew_class_launcher_local,
  public = list(
    #' @field cmdargs (`character()`)\cr
    #' Arguments for R session.
    cmdargs = "",

    initialize = function(
      name = NULL,
      seconds_interval = NULL,
      seconds_timeout = NULL,
      seconds_launch = NULL,
      seconds_idle = NULL,
      seconds_wall = NULL,
      seconds_exit = NULL,
      tasks_max = NULL,
      tasks_timers = NULL,
      reset_globals = NULL,
      reset_packages = NULL,
      reset_options = NULL,
      garbage_collection = NULL,
      launch_max = NULL,
      tls = NULL,
      processes = NULL,
      local_log_directory = NULL,
      local_log_join = TRUE,
      cmdargs = ""
    ) {
      self$cmdargs = cmdargs
      super$initialize(
        name = name,
        seconds_interval = seconds_interval,
        seconds_timeout = seconds_timeout,
        seconds_launch = seconds_launch,
        seconds_idle = seconds_idle,
        seconds_wall = seconds_wall,
        seconds_exit = seconds_exit,
        tasks_max = tasks_max,
        tasks_timers = tasks_timers,
        reset_globals = reset_globals,
        reset_packages = reset_packages,
        reset_options = reset_options,
        garbage_collection = garbage_collection,
        launch_max = launch_max,
        tls = tls,
        processes = processes,
        local_log_directory = local_log_directory,
        local_log_join = local_log_join
      )
    },

    launch_worker = function(call, name, launcher, worker, instance) {
      bin <- file.path(R.home("bin"), "R")
      processx::process$new(
        command = bin,
        args = c(self$cmdargs, "-e", call),
        cleanup = FALSE
      )
    },

    terminate_worker = function(handle) {
      handle$signal(crew::crew_terminate_signal())
    }
  )
)

#' @title Create a controller with a launcher that passes arguments to local R sessions
#'
#' @family local_args
#' @export
#' @description Create an `R6` object to submit tasks and
#'   launch workers.  Command line arguments to the R sessions can be passed
#'   to the workers.
#' @inheritParams crew::crew_controller_local
#' @param cmdargs A character vector of commandline arguments to the R sessions
#'    used for workers.
#'


crewargs_controller_local_args <- function(
    name = "controller name",
    workers = 1L,
    host = NULL,
    port = NULL,
    tls = crew::crew_tls(),
    seconds_interval = 0.5,
    seconds_timeout = 30,
    seconds_launch = 30,
    seconds_idle = Inf,
    seconds_wall = Inf,
    retry_tasks = TRUE,
    tasks_max = Inf,
    tasks_timers = 0L,
    reset_globals = TRUE,
    reset_packages = FALSE,
    reset_options = FALSE,
    garbage_collection = FALSE,
    launch_max = 5L,
    cmdargs = ""
) {
  client <- crew::crew_client(
    name = name,
    workers = workers,
    host = host,
    port = port,
    tls = tls,
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout,
    retry_tasks = retry_tasks
  )
  launcher <- crewargs_class_launcher_local_args$new(
    name = name,
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout,
    seconds_launch = seconds_launch,
    seconds_idle = seconds_idle,
    seconds_wall = seconds_wall,
    tasks_max = tasks_max,
    tasks_timers = tasks_timers,
    reset_globals = reset_globals,
    reset_packages = reset_packages,
    reset_options = reset_options,
    garbage_collection = garbage_collection,
    launch_max = launch_max,
    tls = tls,
    cmdargs = cmdargs
  )
  controller <- crew::crew_controller(client = client, launcher = launcher)
  controller$validate()
  controller
}

