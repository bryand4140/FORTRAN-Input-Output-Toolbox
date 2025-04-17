function read_and_plot_std_output(filename)
    %READ_AND_PLOT_STD_OUTPUT  Read a Fortran std_output file and plot it.
    %
    %   read_and_plot_std_output(FILENAME) reads a file produced by
    %   write_std_output (with "# Title:", "# X-Label:", etc.),
    %   then plots columns 2…end vs. column 1, applying the title,
    %   axis labels, grid and legend flags.
    
    %--- open and init metadata
    fid = fopen(filename,'r');
    if fid<0, error('Could not open file %s', filename); end

    title_str   = '';
    x_label     = '';
    y_label     = '';
    data_labels = {};
    grid_on     = false;
    legend_on   = false;

    %--- parse header comments
    while true
        tline = fgetl(fid);
        if ~ischar(tline), error('Unexpected EOF before header.'); end
        tline = strtrim(tline);
        if isempty(tline), continue; end

        if startsWith(tline,'#')
            % remove leading '#'
            meta = strtrim(extractAfter(tline,1));
            if startsWith(meta,'Title:')
                title_str = strtrim(extractAfter(meta,'Title:'));
            elseif startsWith(meta,'X-Label:')
                x_label = strtrim(extractAfter(meta,'X-Label:'));
            elseif startsWith(meta,'Y-Label:')
                y_label = strtrim(extractAfter(meta,'Y-Label:'));
            elseif startsWith(meta,'DataLabels:')
                lbls = strtrim(extractAfter(meta,'DataLabels:'));
                data_labels = strsplit(lbls,',');
            elseif startsWith(meta,'GridOn:')
                grid_on = strcmpi(strtrim(extractAfter(meta,'GridOn:')),'true');
            elseif startsWith(meta,'LegendOn:')
                legend_on = strcmpi(strtrim(extractAfter(meta,'LegendOn:')),'true');
            end
        else
            % first non-comment line is the column header
            header = strsplit(tline,',');
            break
        end
    end

    %--- read the numeric block
    fmt = repmat('%f',1,numel(header));
    C = textscan(fid, fmt, 'Delimiter',',', 'CollectOutput',true);
    data = C{1};
    fclose(fid);

    %--- extract x & y
    x = data(:,1);
    Y = data(:,2:end);

    %--- plot
    figure;
    hold on;
    for k = 1:size(Y,2)
        plot(x, Y(:,k));
    end
    hold off;

    %--- annotate
    if ~isempty(title_str), title(title_str); end
    if ~isempty(x_label),     xlabel(x_label);   end
    if ~isempty(y_label),     ylabel(y_label);   end
    if grid_on,   grid on;    else    grid off;   end
    if legend_on && numel(data_labels)>1
        % legend for columns 2…end
        legend(data_labels(2:end), 'Location','best');
    end

end
    