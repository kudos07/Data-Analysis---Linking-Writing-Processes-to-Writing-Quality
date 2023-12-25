library(jsonlite)
library(data.table)
set.seed(42)


calculate_production_rate <- function(recording) {
    start_time <- recording[1, 'down_time']
    end_time <- recording[nrow(recording), 'up_time']
    total_time_minutes <- (end_time - start_time) / (1000 * 60)

    characters_process <- 0
    for (i in 1:nrow(recording)) {
        if (recording[i, 'activity'] %in% c('Input', 'Paste', 'Replace')) {
            characters_process <- characters_process + 1
        }
    }
    characters_per_minute_process <- characters_process / total_time_minutes
    words_per_minute_product <- recording[nrow(recording), 'word_count'] / total_time_minutes

    return(list(
        characters_per_minute_process = characters_per_minute_process,
        words_per_minute_product = words_per_minute_product
    ))
}

calculate_pauses <- function(recording) {
    pause_threshold <- 2000

    IKIs <- numeric(nrow(recording) - 1)
    for (i in 2:nrow(recording)) {
        IKIs[i - 1] <- recording[i, 'down_time'] - recording[i - 1, 'up_time']
    }

    pauses <- IKIs[IKIs > pause_threshold]
    number_of_pauses <- length(pauses)

    total_writing_time <- recording[nrow(recording), 'up_time'] - recording[1, 'down_time']
    total_time_minutes <- total_writing_time / (1000 * 60)

    total_pause_time <- sum(pauses)
    proportion_of_pause_time <- (total_pause_time / total_writing_time) * 100

    mean_pause_length <- if (length(pauses) > 0) mean(pauses) else 0
    pause_frequency <- number_of_pauses / total_time_minutes

    return(list(
        number_of_pauses = number_of_pauses,
        proportion_of_pause_time = proportion_of_pause_time,
        mean_pause_length = mean_pause_length,
        pause_frequency = pause_frequency
    ))
}


calculate_revisions <- function(recording) {
    deletions_count <- insertions_count <- 0
    deletions_length <- insertions_length <- 0
    deletions_time <- insertions_time <- 0

    for (i in 1:nrow(recording)) {
        event_activity <- as.character(recording[i, 'activity'])
        event_text_change <- as.character(recording[i, 'text_change'])
        event_up_time <- as.numeric(recording[i, 'up_time'])
        event_down_time <- as.numeric(recording[i, 'down_time'])

        if (event_activity == 'Remove/Cut') {
            deletions_count <- deletions_count + 1
            deletions_length <- deletions_length + nchar(event_text_change)
            deletions_time <- deletions_time + (event_up_time - event_down_time)
        } else if (event_activity %in% c('Input', 'Paste', 'Replace')) {
            insertions_count <- insertions_count + 1
            insertions_length <- insertions_length + nchar(event_text_change)
            insertions_time <- insertions_time + (event_up_time - event_down_time)
        }
    }

    total_writing_time <- recording[nrow(recording), 'up_time'] - recording[1, 'down_time']
    proportion_of_deletions <- (deletions_time / total_writing_time) * 100
    proportion_of_insertions <- (insertions_time / total_writing_time) * 100

    return(list(
        number_of_deletions = deletions_count,
        number_of_insertions = insertions_count,
        length_of_deletions = deletions_length,
        length_of_insertions = insertions_length,
        proportion_of_deletions = proportion_of_deletions,
        proportion_of_insertions = proportion_of_insertions
    ))
}

calculate_bursts <- function(recording) {
    pause_threshold <- 2000
    IKIs <- numeric(nrow(recording) - 1)
    for (i in 2:nrow(recording)) {
        IKIs[i - 1] <- recording[i, 'down_time'] - recording[i - 1, 'up_time']
    }

    p_bursts_count <- r_bursts_count <- 0
    p_bursts_length <- r_bursts_length <- 0
    p_bursts_time <- r_bursts_time <- 0

    current_burst_start <- 1

    for (i in 1:length(IKIs)) {
        if (IKIs[i] > pause_threshold) {
            for (j in current_burst_start:i) {
                p_bursts_length <- p_bursts_length + nchar(as.character(recording[j, 'text_change']))
                p_bursts_time <- p_bursts_time + (as.numeric(recording[j, 'up_time']) - as.numeric(recording[j, 'down_time']))
            }
            p_bursts_count <- p_bursts_count + 1
            current_burst_start <- i + 1
        } else if (recording[i, 'activity'] != 'Input') {
            for (j in current_burst_start:i) {
                r_bursts_length <- r_bursts_length + nchar(as.character(recording[j, 'text_change']))
                r_bursts_time <- r_bursts_time + (as.numeric(recording[j, 'up_time']) - as.numeric(recording[j, 'down_time']))
            }
            r_bursts_count <- r_bursts_count + 1
            current_burst_start <- i + 1
        }
    }

    total_writing_time <- as.numeric(recording[nrow(recording), 'up_time']) - as.numeric(recording[1, 'down_time'])
    proportion_of_p_bursts <- (p_bursts_time / total_writing_time) * 100
    proportion_of_r_bursts <- (r_bursts_time / total_writing_time) * 100

    return(list(
        number_of_p_bursts = p_bursts_count,
        number_of_r_bursts = r_bursts_count,
        length_of_p_bursts = p_bursts_length,
        length_of_r_bursts = r_bursts_length,
        proportion_of_p_bursts = proportion_of_p_bursts,
        proportion_of_r_bursts = proportion_of_r_bursts
    ))
}

calculate_process_variance <- function(recording, intervals = 10) {
    total_writing_time <- recording[nrow(recording), 'up_time'] - recording[1, 'down_time']
    interval_length <- total_writing_time / intervals

    characters_per_interval <- numeric(intervals)
    current_interval <- 1

    for (i in 1:nrow(recording)) {
        event <- recording[i, ]
        while (event['up_time'] > recording[1, 'down_time'] + current_interval * interval_length) {
            current_interval <- current_interval + 1
        }
        characters_per_interval[current_interval] <- characters_per_interval[current_interval] + nchar(event['text_change'])
    }

    characters_per_minute <- sapply(characters_per_interval, function(char_count) char_count / (interval_length / (1000 * 60)))
    std_deviation <- sd(characters_per_minute)

    return(list(
        standard_deviation_per_interval = std_deviation
    ))
}


drop_random <- function(recording, percentage) {
    total_to_drop <- as.integer(nrow(recording) * (percentage / 100))
    indices_to_drop <- sample(1:nrow(recording), total_to_drop)
    dropped <- recording[-indices_to_drop, ]
    return(dropped)
}

drop_conditional <- function(recording) {
    activities_to_drop <- c("Nonproduction", "Replace", "Paste")
    filtered <- recording[!recording$activity %in% activities_to_drop, ]
    return(filtered)
}

apply_drops <- function(recording, drop_functions) {
    for (drop_function in drop_functions) {
        recording <- drop_function(recording)
    }
    return(recording)
}

calculate_all_metrics <- function(recording, calculate_metrics_functions) {
    production_metrics <- as.data.frame(calculate_production_rate(recording))
    pause_metrics <- as.data.frame(calculate_metrics_functions$calculate_pauses(recording))
    revision_metrics <- as.data.frame(calculate_metrics_functions$calculate_revisions(recording))
    burst_metrics <- as.data.frame(calculate_metrics_functions$calculate_bursts(recording))
    process_variance_metrics <- as.data.frame(calculate_metrics_functions$calculate_process_variance(recording))

    metrics <- c(production_metrics, pause_metrics, revision_metrics, burst_metrics, process_variance_metrics)
    return(metrics)
}


process_data_and_create_csv <- function(reorganized_data, calculate_metrics_functions, save_path, drop_percentage = NULL) {
    library(data.table)

    all_metrics <- list()
    total_rows <- nrow(reorganized_data)
    for (i in 1:nrow(reorganized_data)) {
        entry <- reorganized_data[i, ]
        recording_id <- entry[['id']]
        score <- entry[['score']]
        recording <- entry[['recording']][[1]]
        
        if (!is.null(drop_percentage) && drop_percentage != 0) {
          recording <- drop_random(recording, drop_percentage)
        } else {
          if (drop_percentage == 0) {
            print("drop_percentage is 0, no droping.")
          } else {
            recording <- drop_conditional(recording)
          }
        }


        metrics <- calculate_all_metrics(recording, calculate_metrics_functions)
        metrics$recording_id <- recording_id
        metrics$score <- score
        all_metrics <- rbindlist(list(all_metrics, metrics), fill = TRUE)
    }

    fwrite(all_metrics, save_path, row.names = FALSE)
}


reorganized_data <- fromJSON("subset_recordings.json")

calculate_metrics_functions <- list(
    calculate_production_rate = calculate_production_rate,
    calculate_pauses = calculate_pauses,
    calculate_revisions = calculate_revisions,
    calculate_bursts = calculate_bursts,
    calculate_process_variance = calculate_process_variance
)

process_data_and_create_csv(reorganized_data, calculate_metrics_functions, "NR.csv")
process_data_and_create_csv(reorganized_data, calculate_metrics_functions, "V0.csv", 0)
process_data_and_create_csv(reorganized_data, calculate_metrics_functions, "R10.csv", 10)
process_data_and_create_csv(reorganized_data, calculate_metrics_functions, "R20.csv", 20)
process_data_and_create_csv(reorganized_data, calculate_metrics_functions, "R30.csv", 30)