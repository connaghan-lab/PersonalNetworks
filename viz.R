###### Functions related to data visualization

###################### Single Network Visualizations ###########################

#Note: if outputting individual network graphs through pdf() function begins giving
#  errors of invalid fonts, this may be caused by ggraph's fonts not being used.
#  You may need to change font family under theme_graph() to a font listed at this link
#  https://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/postscriptFonts.html

source("metrics.R")

plot_single_network_node_labels <- function(
    tidygra,
    ego_name = NULL,
    fig_title = NULL,
    node_size = 6,
    friend_fill = "#89b53c",
    family_fill = "#007080",
    friend_txt = "white",
    family_txt = "white",
    weak_color = "#236782",
    strong_color = "#c26c21",
    repel = FALSE
) {
    # # # # #
    # Function: Plots a personal network graph using ggraph, distinguishing
    #           between strong and weak ties and highlighting the ego node.
    # Inputs:
    #   tidygra      = A tidygraph object representing a personal network
    #   ego_name     = Character to use instead of "ego" in the graph. Default: NULL (does not replace "ego")
    #   fig_title    = Character to use as the title of the graph. Default: NULL (title is "Record ID: record_id", where "record_id" is extracted from from tidygra)
    #   friend_fill  = Character color with which to fill nodes that contain the text "friend". Default: "#89b53c"
    #   family_fill  = Character color with which to fill nodes that contain the text "family". Default: "#007080"
    #   friend_txt   = Character color with which to write text in nodes that contain the text "friend". Default: "white"
    #   family_txt   = Character color with which to write text in nodes that contain the text "family". Default: "white"
    #   weak_color   = Character color of line indicating weak connections between nodes. Default: "#236782"
    #   strong_color = Character color of line indicating strong connections between nodes. Default: "#c26c21"
    # Outputs: A ggplot object visualizing the network structure
    # # # # #

    # Test if valid tidygra input
    if (is.null(tidygra) || !"tbl_graph" %in% class(tidygra)) {
        warning("Warning: Graph is missing or invalid. Skipping.")
        return(NA) # Skip invalid graphs
    }

    # Test whether the network is an isolate (no weight edge attribute)
    edge_attributes <- tidygra %>%
        tidygraph::activate(edges) %>%
        tibble::as_tibble()
    if (!"weight" %in% colnames(edge_attributes)) {
        # Plot isolate (ego only)
        tg_plot <- ggraph(tidygra, layout = "fr") +
            geom_node_point(size = 4, color = 'black', show.legend = FALSE) +
            theme_graph(base_family = "Helvetica-Narrow")

        return(tg_plot)
    }

    # Check if ego is present in the graph, and if so, focus layout on ego
    node_names <- unique(tidygra %N>% dplyr::pull(name))

    # Transform tie strength into strong/weak strings and create an alter dummy variable
    tidygra <- tidygra %>%
        tidygraph::activate(edges) %>%
        dplyr::mutate(
            strength_of_tie = ifelse(weight == 1, "weak", "strong")
        ) %>%
        tidygraph::activate(nodes) %>%
        dplyr::mutate(
            alter_dummy = ifelse(name != "ego", 1, 0),
            node_fill = dplyr::case_when(
                name == "ego" ~ "black",
                grepl("^Friend", name) ~ friend_fill,
                grepl("^Family", name) ~ family_fill,
                .default = "white"
            ),
            node_text_color = dplyr::case_when(
                name == "ego" ~ "white",
                grepl("^Friend", name) ~ friend_txt,
                grepl("^Family", name) ~ family_txt,
                .default = "black"
            )
        )

    record_id <- tidygra %N>% dplyr::pull(record_id) %>% unique() %>% .[1]
    ttl <- ifelse(is.null(fig_title), paste("Record ID:", record_id), fig_title)

    if ("ego" %in% node_names) {
        focus_index <- which(node_names == "ego")

        # Change "ego" name if a new one was passed
        if (!is.null(ego_name)) {
            tidygra <- tidygra %N>%
                dplyr::mutate(
                    name = ifelse(row_number() == focus_index, ego_name, name)
                )
        }

        # Plot network with ego as focal point
        tg_plot <- ggraph(tidygra, layout = "focus", focus = focus_index) +
            geom_edge_link(
                aes(color = strength_of_tie, linetype = strength_of_tie),
                edge_width = 0.75,
                show.legend = FALSE
            ) +
            scale_edge_linetype_manual(
                values = c("weak" = "dashed", "strong" = "solid")
            ) +
            scale_edge_color_manual(
                values = c("weak" = weak_color, "strong" = strong_color)
            ) +
            geom_node_point(
                aes(color = factor(alter_dummy)),
                size = node_size,
                show.legend = FALSE,
            ) +
            # scale_colour_manual(values = c('black', 'grey66')) +
            geom_node_label(
                aes(label = name, fill = node_fill, color = node_text_color),
                size = node_size,
                label.padding = unit(0.25, "lines"),
                label.size = 0.5,
                show.legend = FALSE,
                repel = repel
            ) +
            scale_color_identity() +
            scale_fill_identity() +
            ggtitle(ttl) +
            theme_graph(base_family = "Helvetica-Narrow") +
            scale_x_continuous(expand = expansion(.15)) +
            scale_y_continuous(expand = expansion(.25))
    } else {
        # Plot network without ego as focal point
        tg_plot <- ggraph(tidygra, layout = "fr") +
            geom_edge_link(
                aes(color = strength_of_tie, linetype = strength_of_tie),
                edge_width = 0.75,
                show.legend = FALSE
            ) +
            scale_edge_linetype_manual(
                values = c("weak" = "dashed", "strong" = "solid")
            ) +
            scale_edge_color_manual(
                values = c("weak" = weak_color, "strong" = strong_color)
            ) +
            geom_node_point(
                size = node_size,
                color = 'grey66',
                show.legend = FALSE
            ) +
            geom_node_label(
                aes(label = name, fill = node_fill, color = node_text_color),
                size = node_size,
                label.padding = unit(0.25, "lines"),
                label.size = 0.5,
                show.legend = FALSE
            ) +
            scale_color_identity() +
            scale_fill_identity() +
            ggtitle(ttl) +
            theme_graph(base_family = "Helvetica-Narrow")
    }
    return(tg_plot)
}

###################### Network Montage Visualization ##########################

plot_single_network <- function(
    tidygra,
    edge_size = 0.5,
    node_size = 2,
    friend_fill = "#89b53c",
    family_fill = "#007080",
    weak_color = "#236782",
    strong_color = "#c26c21"
) {
    # # # # # # # #
    # Function: Plots a personal network graph using ggraph, distinguishing
    #           between strong and weak ties and highlighting the ego node.
    # Inputs:
    #   tidygra      = A tidygraph object representing a personal network
    #   edge_size    = Numeric width of the edges. Default: 0.5
    #   node_size    = Numeric size of the nodes. Default: 2
    #   friend_fill  = Character color with which to fill nodes that contain the text "friend". Default: "#89b53c"
    #   family_fill  = Character color with which to fill nodes that contain the text "family". Default: "#007080"
    #   weak_color   = Character color of line indicating weak connections between nodes. Default: "#236782"
    #   strong_color = Character color of line indicating strong connections between nodes. Default: "#c26c21"
    # Outputs: A ggplot object visualizing the network structure
    # # # # # # # #

    # Test if valid tidygra input
    if (is.null(tidygra) || !"tbl_graph" %in% class(tidygra)) {
        warning("Warning: Graph is missing or invalid. Skipping.")
        return(NA) # Skip invalid graphs
    }

    # Test whether the network is an isolate (no weight edge attribute)
    edge_attributes <- tidygra %>%
        tidygraph::activate(edges) %>%
        tibble::as_tibble()
    if (!"weight" %in% colnames(edge_attributes)) {
        # Plot isolate (ego only)
        tg_plot <- ggraph(tidygra, layout = "fr") +
            geom_node_point(
                size = node_size,
                color = 'black',
                show.legend = FALSE
            ) +
            theme_graph(plot_margin = unit(c(0, 0, 0, 0), "mm"))
    } else {
        # Check if ego is present in the graph, and if so, focus layout on ego
        node_names <- unique(tidygra %N>% dplyr::pull(name))

        # Transform tie strength into strong/weak strings and create an alter dummy variable
        tidygra <- tidygra %>%
            tidygraph::activate(edges) %>%
            dplyr::mutate(
                strength_of_tie = ifelse(weight == 1, "weak", "strong")
            ) %>%
            tidygraph::activate(nodes) %>%
            dplyr::mutate(
                alter_dummy = ifelse(name != 'ego', 1, 0),
                node_fill = dplyr::case_when(
                    name == "ego" ~ "black",
                    grepl("^Friend", name) ~ friend_fill,
                    grepl("^Family", name) ~ family_fill,
                    .default = "grey"
                ),
            )

        if ("ego" %in% node_names) {
            focus_index <- which(node_names == "ego")

            # Plot network with ego as focal point
            tg_plot <- ggraph(tidygra, layout = "focus", focus = focus_index) +
                geom_edge_link(
                    aes(color = strength_of_tie, linetype = strength_of_tie),
                    edge_width = edge_size,
                    show.legend = FALSE
                ) +
                scale_edge_linetype_manual(
                    values = c("weak" = "dashed", "strong" = "solid")
                ) +
                scale_edge_color_manual(
                    values = c("weak" = weak_color, "strong" = strong_color)
                ) +
                geom_node_point(
                    aes(color = node_fill),
                    size = node_size,
                    show.legend = FALSE
                ) +
                scale_colour_identity() +
                theme_graph(plot_margin = unit(c(0.01, 0.01, 0.01, 0.01), "mm"))
        } else {
            # Plot network without ego as focal point
            tg_plot <- ggraph(tidygra, layout = "fr") +
                geom_edge_link(
                    aes(color = strength_of_tie, linetype = strength_of_tie),
                    edge_width = edge_size,
                    show.legend = FALSE
                ) +
                scale_edge_linetype_manual(
                    values = c("weak" = "solid", "strong" = "solid")
                ) +
                scale_edge_colour_manual(
                    values = c("weak" = weak_color, "strong" = strong_color)
                ) +
                geom_node_point(
                    size = node_size,
                    color = 'grey66',
                    show.legend = FALSE
                ) +
                theme_graph(plot_margin = unit(c(0, 0, 0, 0), "mm"))
        }
    }
    return(tg_plot)
}

plot_prop_singleans <- function(
    df,
    mapping,
    attribute,
    key_name,
    attribute_only_cols = FALSE,
    palette = "Dark2",
    plot_type = "pie"
) {
    # # # # # # # #
    # Function: Plots either tiled pie charts or a grouped bar chart showing the proportion
    #           of each possible answer option for each participant in df.
    #           NOTE: Pie charts are generally discouraged in dataviz,
    #           but it is often the better/more informative choice of the two offered here
    # Inputs:
    #   df                  = A personal network dataframe with rows for each subject
    #   mapping             = `named double` corresponding labels (names) and values (numeric) for the attribute
    #   attribute           = `string` attribute in which categories can be found (e.g. "relat")
    #   key_name            = `string` the name to label the legend associated with the mapping options (legend title).
    #   attribute_only_cols = `logical` if TRUE, columns are matched by only attributeNUM. If FALSE: nameAttributeNUM. Default: FALSE
    #   palette             = `string` the BREWER color palette to use in the plots. "Dark2"
    #   plot_type           = `string` indicating the plot type (either "pie" or "grouped_bar"). Default = "pie"
    # Outputs: A ggplot
    # # # # # # # #

    n_alters <- n_alters_with_data(df)
    p_names <- sprintf("Participant %s -- n = %s", df$record_id, n_alters)

    # Get proportions for every mapping value
    props <- purrr::map(names(mapping), \(category) {
        prop_alters_singleans(
            df,
            category,
            mapping,
            attribute,
            attribute_only_cols
        )
    }) %>%
        setNames(names(mapping)) %>%
        dplyr::bind_cols() %>%
        t() %>%
        as.data.frame() %>%
        setNames(p_names) %>%
        tibble::rownames_to_column(key_name)

    # To prevent errors if there are more categories than options  in the palette
    # (less likely with pie charts)
    color_vals <- colorRampPalette(RColorBrewer::brewer.pal(8, name = palette))(
        ncol(props) - 1
    )

    # Make plot
    plt <- switch(
        plot_type,
        # Make list of pie charts
        "pie" = lapply(
            p_names,
            \(column) {
                # Make fig
                ggplot(
                    props,
                    aes(
                        x = "",
                        y = !!ensym(column),
                        fill = !!ensym(key_name)
                    )
                ) +
                    geom_bar(stat = "identity", width = 1, color = "white") +
                    coord_polar("y", start = 0) +
                    scale_fill_manual(values = color_vals) +
                    theme_void() +
                    ggtitle(column)
            }
        ) %>%
            # Tile the list (TODO: consider doing with facet_wrap instead)
            gridExtra::grid.arrange(
                grobs = .,
                nrow = ceiling(length(p_names) / 2)
            ),
        # Grouped barchart (doesn't always make sense for this data
        # but it's the beginning of an alternative option to a pie chart)
        "grouped_bar" = props %>%
            pivot_longer(
                cols = -any_of(key_name),
                names_to = "Participant",
                values_to = "Proportion"
            ) %>%
            ggplot(
                aes(fill = Participant, y = Proportion, x = !!ensym(key_name))
            ) +
            geom_bar(position = "dodge", stat = "identity") +
            scale_fill_manual(values = color_vals),
        # Error if unknown plot type
        stop(sprintf("Unknown plot_type argument: %s", plot_type))
    )

    return(plt)
}

plot_prop_multians_piecharts <- function(df, mapping, attribute, key_name) {
    # # # # # # # #
    # Function: Builds a list of pie charts for each alter for each possible answer in attribute
    #           NOTE: The utility of this function is rather limited and
    #           likely requires changes to make it genuinely helpful for analysis.
    #           Consider tiling the layout of the output, at least.
    #           Also probably worth reversing the order so participants are the leading index
    # Inputs:
    #   df        = A personal network dataframe with rows for each subject
    #   mapping   = `named double` corresponding labels (names) and values (numeric) for the attribute
    #   attribute = `string` attribute in which categories can be found (e.g. "relat")
    #   key_name  = `string` the name to label the legend associated with the mapping options (legend title).
    # Outputs: A list of 15 (each possible alter), each of those containing a pie charts for each participant
    #           That is, participant 5, alter 2 can be accessed by: output[[2]][[5]]
    # # # # # # # #

    n_alters <- n_alters_with_data(df)
    p_names <- sprintf("Participant %s -- n = %s", df$record_id, n_alters)

    # For each alter
    out_list <- vector(mode = "list", length = 15)
    for (alter_num in 1:15) {
        # Get all of the answers for this attribute
        m <- df %>%
            select(matches(sprintf(
                "^name%s%s_+\\d+$",
                alter_num,
                attribute
            ))) %>%
            dplyr::rename_with(
                ~ names(mapping[mapping == as.integer(gsub("(.*_+)", "", .x))])
            )
        # Get proportion and adjust table
        m <- (m / rowSums(m, na.rm = TRUE)) %>%
            t() %>%
            as.data.frame() %>%
            setNames(p_names) %>%
            tibble::rownames_to_column(key_name)

        # Make piechart
        out_list[[alter_num]] <- lapply(
            p_names,
            \(column) {
                ggplot(
                    m,
                    aes(
                        x = "",
                        y = !!ensym(column),
                        fill = !!ensym(key_name)
                    )
                ) +
                    geom_bar(
                        stat = "identity",
                        width = 1,
                        color = "white"
                    ) +
                    coord_polar("y", start = 0) +
                    scale_fill_brewer(palette = palette) +
                    theme_void() +
                    ggtitle(column)
            }
        )
    }
    return(out_list)

    # gridExtra::grid.arrange(grobs = rev(out_list[[1]]), ncol = 1)
}


plot_multians_scatter <- function(
    df,
    mapping,
    attribute,
    record_id_gsub,
    y_name = NULL,
    keep_alter_nums = TRUE,
    point_color = "blue",
    point_size = 5
) {
    # # # # # # # #
    # Function: Plots a tiled scatterplot for each participant
    #            showing all attributes associated with each of their alter for this attribute
    # Inputs:
    #   df              = A personal network dataframe with rows for each subject
    #   mapping         = `named double` corresponding labels (names) and values (numeric) for the attribute
    #   attribute       = `string` attribute in which categories can be found (e.g. "relat")
    #   record_id_gsub  = `string` indicating the regex to use to sort the record_id column.
    #                       E.g., if record_id is "P1", "P24", "P3", etc., "P" be removed before sorting.
    #                       If it's "701-001", "701-024", "701-003", etc., ".*-" should be removed before sorting.
    #   y_name          = `string` the label of the y-axis. No display if NULL. Default: NULL.
    #   keep_alter_nums = `logical` if TRUE, keeps alter numbers as they are (e.g., 1, 2, 6, 7).
    #                       If FALSE, alters are labeled sequentially. Default: TRUE
    #   point_color     = `string` indicating the color of scatterplot points. Default: "blue"
    #   point_size      = `numeric` indicating the size of the scatterplot points. Default: 5
    # Outputs: A ggplot
    # # # # # # # #

    # Allow for no y_name value
    if (is.null(y_name)) {
        # Assign a temporary value that will only be used in the interal dataframe
        y_name = "temp_yname"
        # Remove the axis label in the plot
        y_title = element_blank()
    } else {
        y_title = element_text(size = 15)
    }

    # Build the dataframe to plot
    to_plot <- df %>%
        # Sort the df by record_id using the given substitution pattern, but keep the original values
        arrange(mutate(across(record_id, \(x) {
            as.numeric(gsub(pattern = record_id_gsub, replacement = "", x))
        }))) %>%
        # Get all the answers
        select(matches(sprintf(
            "^name\\d+%s_+\\d+$|^record_id$",
            attribute
        ))) %>%
        # Reshape to columns: Participant | Alter | y_name | TF
        rename(Participant = record_id) %>%
        pivot_longer(
            -Participant,
            names_to = c("Alter", y_name),
            values_to = "TF",
            names_pattern = "name(\\d+).*_+(\\d+)",
            names_transform = list(y_name = as.integer)
        ) %>%
        # Apply recoding via mapping and make other cols factors (needed for plotting)
        mutate(
            !!ensym(y_name) := dplyr::recode(
                !!ensym(y_name),
                !!!setNames(names(mapping), mapping)
            ),
            Participant = factor(
                paste("Participant", Participant),
                levels = unique(paste("Participant", Participant))
            ),
            Alter = factor(Alter, levels = unique(Alter)),
        ) %>%
        # Only take "checked boxes"
        filter(TF == 1)

    # Make plot
    plt <- to_plot %>%
        ggplot(aes(x = Alter, y = !!ensym(y_name))) +
        geom_point(size = point_size, color = point_color)

    if (!keep_alter_nums) {
        # Don't mark exact alter numbers and just label them sequentally
        plt <- plt +
            scale_x_discrete(labels = seq(1, max(as.numeric(to_plot$Alter))))
    }

    # Final touches and wrapping
    # free_x allows for different number of alters in each subject
    plt <- plt +
        theme_bw() +
        theme(
            axis.title = element_text(size = 15),
            axis.title.y = y_title
        ) +
        facet_wrap(~Participant, scales = "free_x")

    return(plt)
}
