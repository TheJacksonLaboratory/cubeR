
//var logDebug = function(){};
var logDebug = console.info.bind(window.console);
var logInfo = console.info.bind(window.console);
var logError = console.error.bind(window.console);

//var _API_R_BASE = 'http://ctlin0098.jax.org/api-102/';
var _API_R_BASE = 'http://localhost/churchilllab/';

/**
 * Create a global getSVG method that takes an array of charts as an argument. The SVG is returned as an argument in the callback.
 */
Highcharts.getSVG = function (charts, options, callback) {
    console.log('getSVG options=', options);
    var svgArr = [],
        top = 0,
        width = 0,
        addSVG = function (svgres) {
            // Grab width/height from exported chart
            let svgWidth = svgres.match(
                /^<svg[^>]*width\s*=\s*\"?(\d+)\"?[^>]*>/
            );
            svgWidth = +svgWidth[1];

            let svgHeight = svgres.match(
                /^<svg[^>]*height\s*=\s*\"?(\d+)\"?[^>]*>/
            );
            svgHeight = +svgHeight[1];

            // Offset the position of this chart in the final SVG
            let svg = svgres.replace('<svg', '<g transform="translate(0,' + top + ')" ');
            svg = svg.replace('</svg>', '</g>');
            top += svgHeight;
            width = Math.max(width, svgWidth);
            svgArr.push(svg);
        },
        exportChart = function (i) {
            if (i === charts.length) {
                return callback('<svg height="' + top + '" width="' + width +
                  '" version="1.1" xmlns="http://www.w3.org/2000/svg">' + svgArr.join('') + '</svg>');
            }
            charts[i].getSVGForLocalExport(options, {}, function () {
                console.log("Failed to get SVG");
            }, function (svg) {
                addSVG(svg);
                return exportChart(i + 1); // Export next only when this SVG is received
            });
        };
    exportChart(0);
};

/**
 * Create a global exportCharts method that takes an array of charts as an argument,
 * and exporting options as the second argument
 */
Highcharts.exportCharts = function (charts, options) {
    console.log('options a', options);
    options = Highcharts.merge(Highcharts.getOptions().exporting, options);
    console.log('options b', options);

    // Get SVG asynchronously and then download the resulting SVG
    Highcharts.getSVG(charts, options, function (svg) {

        Highcharts.downloadSVGLocal(svg, options, function (a,b,c) {
            console.log('downloadSVGLocal',options);
            console.log("Failed to export on client side");
            console.log(a,b,c);
        });
    });
};

/**
 * Add a button to the Highcharts export menu.
 * @param {string} mytext - title for button
 * @param {function} myfunction - function to call
 * @return {Object} buttons for highchart export
 */
function appendExportButton(mytext, myfunction){
    // get default Highchart Export buttons
    let defaultButtons = Highcharts.getOptions().exporting.buttons;
    let myButtons = $.extend(true, {}, defaultButtons);

    myButtons.contextButton.menuItems.push({
        text: mytext,
        onclick: myfunction
    });

    return {
        buttons: myButtons
    };
}

// enhance the original "$.ajax" with a retry mechanism
$.ajax = (($oldAjax) => {
    // on fail, retry by creating a new Ajax deferred
    function check(a, b, c){
        logDebug('CHECK', a, b, c);
        let shouldRetry = (b !== 'success' && b !== 'parsererror');
        if( shouldRetry && --this.retries > 0 ) {
            setTimeout(() => {
                $.ajax(this);
            }, this.retryInterval || 100);
        }
    }

    return settings => $oldAjax(settings).always(check);
})($.ajax);


/** USAGE:
 * now we can use the "retries" property if we need to retry on fail
$.ajax({
    type          : 'GET',
    url           : 'http://www.whatever123.gov',
    timeout       : 2000,
    retries       : 3,     //       <-------- Optional
    retryInterval : 2000   //       <-------- Optional
})
// "fail" will only be called once, and not for each retry
.fail(console.warn);
 */


/**
 *  Extended disable function
 */
jQuery.fn.extend({
    disable: function(state) {
        return this.each(function() {
            let $this = $(this);
            if($this.is('input, button, textarea, select')) {
                this.disabled = state;
            } else {
                $this.toggleClass('disabled', state);
            }
        });
    }
});

/**
 * Download data.
 *
 * From d3.js documentation:
 *
 * When a task completes, it must call the provided callback.
 *
 * The first argument to the callback should be null if the task is
 * successful, or the error if the task failed.
 *
 * The optional second argument to the callback is the return value of the task.
 * (To return multiple values from a single callback, wrap the results in an object or array.)
 *
 * @param {string} URL - the URL to download data
 * @param {string} description - descriptive text
 * @param {function} callback - the callback function
 */
function downloadData(URL, description, callback) {
    logInfo('Downloading: ', URL);
    $.ajax({
        url: URL,
        method: 'GET',
    }).done(function(data, textStatus, jqXHR) {
        logInfo(`Download of ${description}`);
        logDebug(`Download of ${description}:`, data);
        callback(null, data);
    }).fail(function(jqXHR, textStatus, errorThrown) {
        logError(description, textStatus);
        logError(description, textStatus, errorThrown);
        callback(errorThrown, null);
    });
}


var QTL = function() {
    var g = {};

    function init() {
        /*
         * Global data
         */
        g.DATASETS = null;         // hash of all datasets
        g.ENSEMBL_RELEASE = null;  // Ensembl release for Ensimpl API
        g.SPECIES_ID = 'Mm';       // TODO: do not hard code species
        g.ENSIMPL = null;          // Ensimple API
        g.CHROMOSOMES = null;      // all chromosome information

        /*
         * current selected information
         */
        g.dataSetID = null;        // selected dataset id
        g.geneID = null;           // selected gene id
        g.proteinID = null;        // selected protein id
        g.phenotypeID = null;      // selected phenotype id
        g.gene = null;             // selected gene information

        /*
         * Interactive stored data
         */
        g.LODAdditiveMax = null;
        g.LODThreshold = null;           // threshold value of LOD scores
        g.LODChartData = null;
        g.interactiveCovariates = null;  // list of covariates for current data set
        g.phenoDynaTable = null;
        g.expressionData = null;
        g.searchTable = null;
        g.orderCount = null;
        g.secondaryPlotMarkerID = null;
        g.secondaryPlotChromosome = null;
        g.secondaryPlotLocation = null;
        g.secondaryPlotCovar = null;

        /*
         * Chart variables
         */
        g.chartPeaks = null;
        g.chartLOD = null;
        g.chartLODCovariateFull = null;
        g.chartLODCovariateDiff = null;
        g.chartMediation = null;
        g.chartEffect = null;
        g.chartSNPAssocSNPS = null;
        g.chartSNPAssocGenes = null;

        // for tasks
        g.runningTask = false;

        g.PROTOCOL = window.location.protocol;

        d3.queue()
            .defer(downloadDataSets)
            .await(function(error, data) {
                logDebug('dataSetsData=', data);

                if (error) {
                    logError(error);
                    // ?debug=1 will prevent forwarding on error
                    window.location = '/error/500';
                    return;
                }

                if ((data.result === undefined) || (data.result.datasets.length === 0)) {
                    logError('data=', data);
                    // ?debug=1 will prevent forwarding on error
                    window.location = '/error/500';
                    return;
                }

                g.DATASETS = {};

                g.ENSEMBL_RELEASE = null;
                if ('ensembl.version' in data.result) {
                    g.ENSEMBL_RELEASE  = data.result['ensembl.version'];
                }
                // the new way is RELEASE, replacing VERSION
                if ('ensembl.release' in data.result) {
                    g.ENSEMBL_RELEASE  = data.result['ensembl.release'];
                }

                $.each(data.result.datasets, function(dsi, ds) {
                    ds.gene_ids = {};
                    ds.protein_ids = {};
                    ds.phenotypes = {};

                    // change the case so we don't have to throughout
                    ds.datatype = ds.datatype.toLowerCase();

                    if (ds.datatype === 'mrna') {
                        // convert array to object (hash)
                        $.each(ds.annotations.ids, function(index, element) {
                            ds.gene_ids[element] = 1;
                        });
                    } else if (ds.datatype === 'protein') {
                        $.each(ds.annotations.ids, function(index, element) {
                            if (!(element['gene.id'] in ds.gene_ids)) {
                                ds.gene_ids[element['gene.id']] = {
                                    'id': element['gene.id'],
                                    'protein_ids':[]
                                };
                            }
                            ds.protein_ids[element['protein.id']] = element['gene.id'];
                            ds.gene_ids[element['gene.id']].protein_ids.push(element['protein.id']);
                        });
                    } else if ((ds.datatype === 'pheno') || (ds.datatype === 'phenotype')) {
                        ds.datatype = 'pheno';

                        $.each(ds.annotations, function(index, element) {
                            ds.phenotypes[element['data.name']] = element;
                        });
                    } else {
                        // TODO: handle error
                        logError('ERROR in dataset data:', ds);
                    }

                    // don't need the extra annotations laying around
                    delete ds.annotations;

                    g.DATASETS[ds.id] = ds;
                });

                let requestedDataSetID = '';


                if (g.DATASETS[requestedDataSetID] !== undefined) {
                    g.dataSetID = requestedDataSetID;
                } else {
                    g.dataSetID = data.result.datasets[0].id;
                }

                // the above if is working, but we need to think through all of
                // the other ramifications of setting this dataset

                configureCovarInfo(true);

                logDebug('data.result.datasets.length=',data.result.datasets.length);

                if (data.result.datasets.length === 1) {
                    // just show a menu item
                    let html = `<ul class="navbar-nav navbar-dark mr-auto">
                                    <li class="nav-item active" style="color:white">
                                        ${g.DATASETS[g.dataSetID]['display.name']}
                                    </li>
                                </ul>`;

                    $('#dataSetSelectMenu').append(html);
                    $('#correlationDatasetSelection').html(`${g.DATASETS[g.dataSetID]['display.name']}`);
                    $('#correlationDatasetSelection').append(`<input type="hidden" id="correlationDatasetSelect" value="${g.DATASETS[g.dataSetID].id}"/>`);

                } else if (data.result.datasets.length > 1) {
                    // show multiple menu items
                    let html = `<button class="navbar-toggler" type="button" data-toggle="collapse" data-target="#navbarTogglerQTL" aria-controls="navbarTogglerQTL" aria-expanded="false" aria-label="Toggle navigation">
                                    <span class="navbar-toggler-icon"></span>
                                </button>
                                <div class="collapse navbar-dark navbar-collapse" id="navbarTogglerQTL">
                                <span class="navbar-text text-white pr-2">Current Data Set <i class="fas fa-long-arrow-alt-right"></i></span>
                                <select data-style="btn-sm btn-light" class="selectpicker" id="datasetSelect">`;

                    for (let d in data.result.datasets) {
                        if (g.dataSetID === data.result.datasets[d].id) {
                            console.log(g.dataSetID, '===', data.result.datasets[d].id);
                            html += `<option value="${data.result.datasets[d].id}" selected>${data.result.datasets[d]['display.name']}</option>`;
                        } else {
                            console.log(g.dataSetID, '!==', data.result.datasets[d].id);
                            html += `<option value="${data.result.datasets[d].id}">${data.result.datasets[d]['display.name']}</option>`;
                        }
                    }

                    html += '</select></div>';

                    $('#dataSetSelectMenu').append(html);

                    $('#datasetSelect').selectpicker();

                    $('#datasetSelect').on('changed.bs.select', function (e, clickedIndex, isSelected, previousValue) {
                        let selected = $(this).find(':selected').val();
                        switchDataSet(selected);
                    });


                    // show multiple menu items
                    let htmlNew = `<select id="correlationDatasetSelect" data-width="100%" data-style="btn-secondary btn-sm" class="selectpicker">`;

                    for (let d in data.result.datasets) {
                        htmlNew += `<option value="${data.result.datasets[d].id}">${data.result.datasets[d]['display.name']}</option>`;
                    }

                    htmlNew += '</select>';

                    $('#correlationDatasetSelection').html(htmlNew);

                    $('#correlationDatasetSelect').selectpicker();

                    $('#correlationDatasetSelect').on('changed.bs.select', function (e, clickedIndex, isSelected, previousValue) {
                        let selected = $(this).find(':selected').val();
                        let interactiveCovariate = $('#interactiveCovarCorrelation').find(':selected').val();
                        switchCorrelationDataSet(selected, interactiveCovariate);
                    });
                }

                g.ENSIMPL = new ensimpl({
                    search_limit: 100,
                    search_release: g.ENSEMBL_RELEASE,
                    search_species: g.SPECIES_ID
                });

                d3.queue()
                    .defer(downloadChromosomes, g.ENSEMBL_RELEASE, g.SPECIES_ID)
                    .defer(downloadLODPeaks, g.dataSetID)
                    .await(function(errorPeaks, chromosomes, lodPeaks) {
                        g.CHROMOSOMES = {'idx': [], 'chr': {}};

                        let configChromosomes = ['1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', 'X'];
                        let start = 1;
                        let end = 0;

                        $.each(chromosomes.chromosomes, function(index, element) {
                            if (configChromosomes.indexOf(element.chromosome) !== -1) {
                                let chrom = element;

                                start = end + 1;
                                end += chrom.length;

                                chrom.start = start;
                                chrom.end = end;
                                chrom.mid = chrom.start + Math.ceil((chrom.end - chrom.start) / 2);

                                g.CHROMOSOMES.idx.push(chrom);
                                g.CHROMOSOMES.chr[element.chromosome] = chrom;
                            }
                        });

                        g.CHROMOSOMES.totalLength = end;
                        g.DATASETS[g.dataSetID].lodpeaks = lodPeaks.result;

                        setDataSet(g.dataSetID, true);

                    });

                // download all the other load peaks from all the datasets except the current one
                // we do this in the background to gain some performance
                let q = d3.queue();
                logDebug('Downloading the peaks...');

                $.each(g.DATASETS, function(key, value) {
                    if (key !== g.dataSetID) {
                        q.defer(downloadLODPeaks, key);
                    }
                });

                q.awaitAll(function(errorAll, results) {
                    // TODO: handle error
                    $.each(results, function(key, value) {
                        g.DATASETS[value.id].lodpeaks = value.result;
                    });

                    window.loading_screen.finish();
                });



            });
        }

    /**
     * Display the gene information.
     * @param {Object} geneData - gene information
     */
    function displayGeneData(geneData) {
        // TODO: link to database based upon ensembl version?

        for(let key in geneData) {
            geneData = geneData[key];
        }

        let startFormat = formatMbp(geneData.start/1000000);
        let endFormat = formatMbp(geneData.end/1000000);
        let mbpDesc = 'Mbp';

        if (startFormat < 1) {
            startFormat = geneData.start;
            endFormat = geneData.end;
            mbpDesc = 'bp';
        }

        let strand = 'forward';
        if (geneData.strand === '-') {
            strand = 'negative';
        }

        let htmlHeader = geneData.id;
        htmlHeader += ` <em>${geneData.symbol}</em>`;

        let htmlBody = '<table class="table table-sm">';
        htmlBody += `<tr><td class="border-0"><strong>ID</strong></td><td class="border-0">${geneData.id}`;
        htmlBody += '<br>';
        htmlBody += `<small><a target="_newwin" href="http://www.ensembl.org/id/${geneData.id}">Ensembl <i class="fas fa-external-link-alt" aria-hidden="true"></i></small></a>`;

        $.each(geneData.external_ids, function(idx, elem) {
            let url = '';
            let urlDesc = '';

            if (elem.db === 'EntrezGene') {
                url = `https://www.ncbi.nlm.nih.gov/gene/?term=${elem.db_id}`;
                urlDesc = `NCBI Gene: ${elem.db_id}`;
            } else if (elem.db === 'MGI') {
                url = `http://www.informatics.jax.org/marker/${elem.db_id}`;
                urlDesc = `${elem.db_id}`;
            } else if (elem.db === 'Uniprot_gn') {
                url = `https://www.uniprot.org/uniprot/${elem.db_id}`;
                urlDesc = `Uniprot: ${elem.db_id}`;
            }

            if (url !== '') {
                htmlBody += ` <small><a target="_newwin" href="${url}">${urlDesc} <i class="fas fa-external-link-alt" aria-hidden="true"></i></small></a>`;
            }

        });

        htmlBody += '</td></tr>';
        htmlBody += `<tr><td><strong>Symbol</strong></td><td>${geneData.symbol}</td></tr>`;
        htmlBody += `<tr><td><strong>Location</strong></td><td>${geneData.chromosome}:${startFormat}-${endFormat}</td></tr>`;

        if (geneData.synonyms !== undefined) {
            let synonyms = geneData.synonyms.join(', ');
            htmlBody += `<tr><td><strong>Synonyms</strong></td><td>${synonyms}</td></tr>`;
        }

        if (geneData.name) {
            htmlBody += `<tr><td><strong>Description</strong></td><td>${geneData.name}</td></tr>`;
        }

        htmlBody += '</table>';

        $('#div_current_item_information_header').html(htmlHeader);
        $('#div_current_item_information_body').html(htmlBody);
    }

    /**
     * Display the pheno information.
     * @param {Object} geneData - gene information
     */
    function displayPhenoData(phenoID) {
        let pheno = g.DATASETS[g.dataSetID].phenotypes[phenoID];
        $('#div_current_item_information_header').html(phenoID);
        $('#div_current_item_information_body').html(pheno.desc);
    }

    function updatePeaks(minValue) {
        logDebug('updatePeaks()');
        let covar = $('#interactiveCovarPeaks').val();

        g.LODThreshold = minValue;
        let newData = [];

        if (g.DATASETS[g.dataSetID].datatype === 'mrna') {
            $.each(g.DATASETS[g.dataSetID].lodpeaks[covar], function (index, element) {
                let markerChr = g.CHROMOSOMES.chr[element[1]];
                let markerPos = element[2] * 1000000.0;
                let geneChr = g.CHROMOSOMES.chr[element[5]];
                let genePos = element[6] * 1000000.0;

                if ((markerChr !== undefined) && (geneChr !== undefined)) {
                    //data.push([+(geneChr.start + genePos), +(markerChr.start + markerPos)]);

                    if (element[7] >= g.LODThreshold) {
                        let d = {
                            geneChr: geneChr,
                            genePos: genePos,
                            genePosOrig: element[2],
                            markerChr: markerChr,
                            markerPos: markerPos,
                            x: +(markerChr.start + markerPos),
                            y: +(geneChr.start + genePos),
                            lod: element[7],
                            markerID: element[0],
                            geneID: element[3],
                            geneSymbol: element[4],
                            covar: covar
                        };

                        if (element.length === 16) {
                            d['A'] = element[8];
                            d['B'] = element[9];
                            d['C'] = element[10];
                            d['D'] = element[11];
                            d['E'] = element[12];
                            d['F'] = element[13];
                            d['G'] = element[14];
                            d['H'] = element[15];
                        }

                        newData.push(d);
                    }
                }
            });

            g.chartPeaks.series[0].setData(newData, true, false, false);

        } else if (g.DATASETS[g.dataSetID].datatype === 'protein') {
            // [marker_id, chrom, position, protein_id, gene_id, symbol, gene_chrom, gene_mid, lod
            // ["1_106129060","1",106.12906,"ENSMUSP00000108356","ENSMUSG00000009907","Vps4b","1",106.7804,8.2928352859],
            $.each(g.DATASETS[g.dataSetID].lodpeaks[covar], function (index, element) {
                let markerChr = g.CHROMOSOMES.chr[element[1]];
                let markerPos = element[2] * 1000000.0;
                let geneChr = g.CHROMOSOMES.chr[element[6]];
                let genePos = element[7] * 1000000.0;

                if ((markerChr !== undefined) && (geneChr !== undefined)) {
                    //data.push([+(geneChr.start + genePos), +(markerChr.start + markerPos)]);

                    if (element[8] >= g.LODThreshold) {
                        let d = {
                            geneChr: geneChr,
                            genePos: genePos,
                            genePosOrig: element[2],
                            markerChr: markerChr,
                            markerPos: markerPos,
                            x: +(markerChr.start + markerPos),
                            y: +(geneChr.start + genePos),
                            lod: element[8],
                            markerID: element[0],
                            proteinID: element[3],
                            geneID: element[4],
                            geneSymbol: element[5],
                            covar: covar
                        };

                        if (element.length === 17) {
                            d['A'] = element[9];
                            d['B'] = element[10];
                            d['C'] = element[11];
                            d['D'] = element[12];
                            d['E'] = element[13];
                            d['F'] = element[14];
                            d['G'] = element[15];
                            d['H'] = element[16];
                        }

                        newData.push(d);
                    }
                }
            });

            g.chartPeaks.series[0].setData(newData, true, false, false);

        } else {
            let seriesData = {};
            let minLOD = Infinity;
            let maxLOD = -Infinity;
            $.each(g.DATASETS[g.dataSetID].lodpeaks[covar], function(index, element) {
                // element[0] = marker_id
                let markerChr = g.CHROMOSOMES.chr[element[1]];
                let markerPos = element[2] * 1000000.0;

                // lookup the phenotype
                let phenotype = g.DATASETS[g.dataSetID].phenotypes[element[3]];

                if (!(phenotype.category in seriesData)) {
                    seriesData[phenotype.category] = [];
                }

                let d = {
                    x: markerChr.start + markerPos,
                    y: element[6],
                    lod: element[6],
                    phenoDataName: element[3],
                    phenoShortName: element[4],
                    phenoDescription: element[5],
                    markerPosOrig: element[2],
                    markerChr: markerChr,
                    markerPos: markerPos,
                    markerID: element[0],
                    covar: covar
                };

                if (element.length === 15) {
                    d['A'] = element[7];
                    d['B'] = element[8];
                    d['C'] = element[9];
                    d['D'] = element[10];
                    d['E'] = element[11];
                    d['F'] = element[12];
                    d['G'] = element[13];
                    d['H'] = element[14];
                }

                seriesData[phenotype.category].push(d);

                minLOD = Math.min(minLOD, element[6]);
                maxLOD = Math.max(maxLOD, element[6]);

            });

            // remove all the series
            while(g.chartPeaks.series.length > 0 ) {
                g.chartPeaks.series[0].remove(false);
            }

            // add new series
            $.each(seriesData, function(index, element) {
                g.chartPeaks.addSeries({
                    data: element,
                    name: index,
                    turboThreshold: 0,
                }, false);
            });

            g.chartPeaks.yAxis[0].update({
                min: minLOD,
                max: maxLOD,
            });

            g.chartPeaks.redraw(true);
        }
    }

    function exportLODPeaks(id, peaks) {

        let covar = $('#interactiveCovarPeaks').val();



        let csvContent = '';

        if (g.DATASETS[g.dataSetID].datatype === 'mrna') {
            csvContent = '"gene_id","symbol","gene_chrom","gene_mid","marker_id","marker_chrom","marker_position","lod"';
            if (peaks[0].length === 16) {
                csvContent += ',"A","B","C","D","E","F","G","H"';
            }
            csvContent += '\n';
            // [marker_id, chrom, position, gene_id, symbol, gene_chrom, gene_mid, lod
            //     0         1       2        3       4         5          6        7
            // ["1_100007442","1",100.0074,"ENSMUSG00000028028","Alpk1","3",127.7255,6.1147]
            $.each(peaks, function(k, v) {
                let line = `"${v[3]}","${v[4]}","${v[5]}",${v[6]},"${v[0]}","${v[1]}",${v[2]},"${v[7]}"`;
                if (peaks[0].length === 16) {
                    line += `,${v[8]},${v[9]},${v[10]},${v[11]},${v[12]},${v[13]},${v[14]},${v[15]}`;
                }
                line += '\n';

                csvContent += line;
            });
        } else if (g.DATASETS[g.dataSetID].datatype === 'protein') {
            csvContent = '"protein_id","gene_id","symbol","gene_chrom","gene_mid","marker_id","marker_chrom","marker_position","lod"';
            if (peaks[0].length === 17) {
                csvContent += ',"A","B","C","D","E","F","G","H"';
            }
            csvContent += '\n';
            // [marker_id, chrom, position, protein_id, gene_id, symbol, gene_chrom, gene_mid, lod
            //     0         1       2          3          4        5         6        7        8
            // ["1_106129060","1",106.12906,"ENSMUSP00000108356","ENSMUSG00000009907","Vps4b","1",106.7804,8.2928352859],
            $.each(peaks, function(k, v) {
                let line = `"${v[3]}","${v[4]}","${v[5]}","${v[6]}",${v[7]},"${v[0]}","${v[1]}",${v[2]},"${v[8]}"\n`;
                if (peaks[0].length === 17) {
                    line += `,${v[9]},${v[10]},${v[11]},${v[12]},${v[13]},${v[14]},${v[15]},${v[16]}`;
                }
                line += '\n';
                csvContent += line;
            });
        } else if (g.DATASETS[g.dataSetID].datatype === 'pheno') {
            csvContent = '"marker_id","marker_chrom","marker_position","phenotype","lod"';
            if (peaks[0].length === 15) {
                csvContent += ',"A","B","C","D","E","F","G","H"';
            }
            csvContent += '\n';
            // ["5_137006393","5",137.0064,"MEcyan","MEcyan","MEcyan",9.1161],[
            //       0         1     2        3        4        5        6
            $.each(peaks, function(k, v) {
                let line = `"${v[0]}","${v[1]}",${v[2]},"${v[4]}",${v[6]}`;
                if (peaks[0].length === 16) {
                    line += `,${v[7]},${v[8]},${v[9]},${v[10]},${v[11]},${v[12]},${v[13]},${v[14]}`;
                }
                line += '\n';
                csvContent += line;
            });
        }

        downloadCSV(csvContent, `${id}_LODPEAKS.csv`, 'text/csv;encoding:utf-8');
    }

    /**
     * Plot LOD Peaks as a scatter plot.
     */
    function plotLODPeaks() {
        logDebug('plotLODPeaks()');
        let covar = $('#interactiveCovarPeaks').val();
        logDebug('covar=', covar);

        // DATA will depend on type
        let ticksStartVals = [];
        let ticksStartText = {};
        let ticksMidVals = [];
        let ticksMidText = {};
        let plotLines = [];
        let plotBands = [];
        let i = 0;

        $.each(g.CHROMOSOMES.chr, function(idx, value) {
            plotLines.push({
               color: '#eeeeee',
               value: value.start,
               width: 2,
            });

            if ((i % 2) === 1) {
                plotBands.push({
                    color: '#eeeeee',
                    from: value.start,
                    to: value.start + value.length
                });
            }

            i += 1;

            ticksStartVals.push(value.start);
            ticksStartText[value.start] = value.chromosome;
            ticksMidVals.push(value.mid);
            ticksMidText[value.mid] = value.chromosome;
        });

        if ((i % 2) === 0) {
            plotBands.push({
                color: '#eeeeee',
                from: g.CHROMOSOMES.idx[i-1].start,
                to: g.CHROMOSOMES.idx[i-1].start + g.CHROMOSOMES.idx[i-1].length
            });
        }

        plotLines.push({
           color: '#eeeeee',
           value: g.CHROMOSOMES.totalLength,
           width: 2,
        });

        let data = [];
        let xAxis = null;
        let yAxis = null;
        let tooltip = null;
        let clickFunction = null;
        let series = [];
        let legend = null;
        let minLOD = Infinity;
        let maxLOD = -Infinity;
        let html = `<div class="row">
                        <div class="col">
                            <div id="plotLodPeaks" style="width: 100%"></div>
                        </div>
                    </div>`;

        g.LODThreshold = null;

        let exporting = appendExportButton('Download CSV', function() {
            exportLODPeaks(g.DATASETS[g.dataSetID]['display.name'], g.DATASETS[g.dataSetID].lodpeaks[covar]);
        });
        exporting.filename = g.DATASETS[g.dataSetID]['display.name'] + '_PEAKS';


        //TODO: make sure g.LOD is defined above and g.chartPeaks and reset appropriately

        if (g.DATASETS[g.dataSetID].datatype === 'mrna') {
            // [marker_id, chrom, position, gene_id, symbol, gene_chrom, gene_mid, lod
            // ["1_100007442","1",100.0074,"ENSMUSG00000028028","Alpk1","3",127.7255,6.1147]
            $.each(g.DATASETS[g.dataSetID].lodpeaks[covar], function(index, element) {
                let markerChr = g.CHROMOSOMES.chr[element[1]];
                let markerPos = element[2] * 1000000.0;
                let geneChr = g.CHROMOSOMES.chr[element[5]];
                let genePos = element[6] * 1000000.0;

                if ((markerChr !== undefined) && (geneChr !== undefined)) {
                    //data.push([+(geneChr.start + genePos), +(markerChr.start + markerPos)]);
                    data.push({
                        geneChr: geneChr,
                        genePos: genePos,
                        genePosOrig: element[6],
                        markerChr: markerChr,
                        markerPos: markerPos,
                        markerPosOrig: element[2],
                        x: +(markerChr.start + markerPos),
                        y: +(geneChr.start + genePos),
                        lod: element[7],
                        markerID: element[0],
                        geneID: element[3],
                        geneSymbol: element[4],
                    });

                    minLOD = Math.min(minLOD, element[7]);
                    maxLOD = Math.max(maxLOD, element[7]);
                }
            });

            minLOD = Math.max(0, Math.floor(minLOD));

            // maxLOD will be 80% of max lod score
            // maxLOD = maxLOD * 0.8;

            // round up to nearest 10, 11->20, 7->10, 88->90
            maxLOD = Math.ceil(maxLOD / 10) * 10;

            let currentLOD = minLOD;

            logDebug('minLOD=', minLOD, 'maxLOD=', maxLOD);

            html = html + `
                    <div class="row">
                        <div class="col-sm-auto my-auto margin y auto font-weight-bold">
                            <span class="align-bottom">LOD Threshold: </span>
                        </div>
                        <div class="col-auto">
                            <div class="input-group">
                                <input id="lodSliderValue" type="number" min="${minLOD}" max="${maxLOD}" step=".5" value="${currentLOD}" class="form-control" size="6">
                                <span class="input-group-append">
                                    <button id="btnGoLOD" class="btn btn-primary"><i class="fas fa-caret-right"></i></button>
                                </span>
                            </div>
                        </div>
                    </div>
                    <div class="row justify-content-center">
                        <div class="col text-right">
                            <span>${minLOD}</span>
                        </div>
                        <div class="col-8">
                            <input type="range" class="custom-range" value="${currentLOD}" min="${minLOD}" max="${maxLOD}" id="lodRange">
                        </div>
                        <div class="col text-left">
                            <span>${maxLOD}</span>
                        </div>
                    </div>
`;

            $('#divLODPeaks').html(html);

            $('#lodRange').on('change', function(evt) {
                $('#lodSliderValue').val(evt.target.value);
                updatePeaks(evt.target.value);
            });

            $('#btnGoLOD').click(function(evt) {
                $('#lodRange').val(+$('#lodSliderValue').val());
                updatePeaks(+$('#lodSliderValue').val());
            });


            $('#lodSliderValue').bind('keyup input', function(evt){
                $('#lodRange').val(+$('#lodSliderValue').val());
                updatePeaks(+$('#lodSliderValue').val());
            });


            series = [{
                data: data,
                // When a series contains a data array that is
                // longer than this, only one dimensional arrays
                // of numbers, or two dimensional arrays with
                // x and y values are allowed.
                // Also, only the first point is tested, and
                // the rest are assumed to be the same format.
                // This saves expensive data checking and
                // indexing in long series. Set it to 0 disable.
                // Defaults to 1000.
                turboThreshold: 0,
                //boostThreshold: 0,
                //fillOpacity: 0.1,
                //color: 'rgba(223, 83, 83, 0.5)',
            }];

            legend = {
                enabled: false
            };

            xAxis = {
                endOnTick: false,
                gridLineWidth: 0,
                labels: {
                    formatter: function () {
                        return ticksMidText[this.value];
                    }
                },
                min: 0,
                max: g.CHROMOSOMES.totalLength,
                plotLines: plotLines,
                showLastLabel: true,
                startOnTick: false,
                tickPositions: ticksMidVals,
                title: {
                    enabled: true,
                    text: 'Marker'
                },
            };

            yAxis = {
                endOnTick: false,
                gridLineWidth: 0,
                labels: {
                    formatter: function () {
                        return ticksMidText[this.value];
                    }
                },
                min: 0,
                max: g.CHROMOSOMES.totalLength,
                plotLines: plotLines,
                showLastLabel: true,
                startOnTick: false,
                tickPositions: ticksMidVals,
                title: {
                    enabled: true,
                    text: 'Gene'
                },
            };

            clickFunction = function (event) {
                logDebug('clickFunction:', this.geneID, null, g.dataSetID, this.covar);
                selectGeneProtein(this.geneID, null, g.dataSetID, this.covar);
            };

            tooltip = {
                followPointer: false,
                outside: true,
                formatter: function (a) {
                    let ret = `LOD: ${this.point.lod}<br>Gene ID: ${this.point.geneID}<br>Symbol: ${this.point.geneSymbol}<br>Gene Location: ${this.point.geneChr.chromosome}:${this.point.genePos}<br>Marker ID: ${this.point.markerID}<br>Marker Location: ${this.point.markerChr.chromosome}:${this.point.markerPos}`;
                    if ('A' in this.point) {
                        ret += `<br>Allele Effects:<br>`;
                        ret += `<br> A: ${this.point.A}`;
                        ret += `<br> B: ${this.point.B}`;
                        ret += `<br> C: ${this.point.C}`;
                        ret += `<br> D: ${this.point.D}`;
                        ret += `<br> E: ${this.point.E}`;
                        ret += `<br> F: ${this.point.F}`;
                        ret += `<br> G: ${this.point.G}`;
                        ret += `<br> H: ${this.point.H}`;
                    }
                    return ret;

                }
            };
        } else if (g.DATASETS[g.dataSetID].datatype === 'protein') {
            // [marker_id, chrom, position, protein_id, gene_id, symbol, gene_chrom, gene_mid, lod
            // ["1_106129060","1",106.12906,"ENSMUSP00000108356","ENSMUSG00000009907","Vps4b","1",106.7804,8.2928352859],
            $.each(g.DATASETS[g.dataSetID].lodpeaks[covar], function(index, element) {

                let markerChr = g.CHROMOSOMES.chr[element[1]];
                let markerPos = element[2] * 1000000.0;
                let geneChr = g.CHROMOSOMES.chr[element[6]];
                let genePos = element[7] * 1000000.0;

                if ((markerChr !== undefined) && (geneChr !== undefined)) {
                    //data.push([+(geneChr.start + genePos), +(markerChr.start + markerPos)]);

                    data.push({
                        geneChr: geneChr,
                        genePos: genePos,
                        genePosOrig: element[7],
                        markerChr: markerChr,
                        markerPos: markerPos,
                        markerPosOrig: element[2],
                        x: +(markerChr.start + markerPos),
                        y: +(geneChr.start + genePos),
                        lod: element[8],
                        markerID: element[0],
                        proteinID: element[3],
                        geneID: element[4],
                        geneSymbol: element[5],
                        covar: covar
                    });

                    minLOD = Math.min(minLOD, element[8]);
                    maxLOD = Math.max(maxLOD, element[8]);
                }
            });

            minLOD = Math.max(0, Math.floor(minLOD));

            // maxLOD will be 80% of max lod score
            // maxLOD = maxLOD * 0.8;

            // round up to nearest 10, 11->20, 7->10, 88->90
            maxLOD = Math.ceil(maxLOD / 10) * 10;

            let currentLOD = minLOD;

            html = html + `
                <div class="row">
                    <div class="col-sm-auto my-auto margin y auto font-weight-bold">
                        <span class="align-bottom">LOD Threshold: </span>
                    </div>
                    <div class="col-sm-auto">
                        <div class="input-group">
                            <input id="lodSliderValue" type="number" min="${minLOD}" max="${maxLOD}" step=".5" value="${currentLOD}" class="form-control" size="6">
                            <span class="input-group-append">
                                <button id="btnGoLOD" class="btn btn-primary"><i class="fas fa-caret-right"></i></button>
                            </span>
                        </div>
                    </div>
                </div>
                <div class="row">
                    <div class="col-2 text-right">
                        <span>${minLOD}</span>
                    </div>
                    <div class="col-8">
                        <input type="range" class="custom-range" value="${currentLOD}" min="${minLOD}" max="${maxLOD}" id="lodRange">
                    </div>
                    <div class="col-2 text-left">
                        <span>${maxLOD}</span>
                    </div>
                </div>`;


            $('#divLODPeaks').html(html);

            $('#lodRange').on('change', function(evt) {
                $('#lodSliderValue').val(evt.target.value);
                updatePeaks(evt.target.value);
            });

            $('#btnGoLOD').click(function(evt) {
                $('#lodRange').val(+$('#lodSliderValue').val());
                updatePeaks(+$('#lodSliderValue').val());
            });


            $('#lodSliderValue').bind('keyup input', function(evt){
                $('#lodRange').val(+$('#lodSliderValue').val());
                updatePeaks(+$('#lodSliderValue').val());
            });

            series = [{
                // When a series contains a data array that is
                // longer than this, only one dimensional arrays
                // of numbers, or two dimensional arrays with
                // x and y values are allowed.
                // Also, only the first point is tested, and
                // the rest are assumed to be the same format.
                // This saves expensive data checking and
                // indexing in long series. Set it to 0 disable.
                // Defaults to 1000.
                turboThreshold: 0,
                //boostThreshold: 0,
                //fillOpacity: 0.1,
                //color: 'rgba(223, 83, 83, 0.5)',
                data: data
            }];

            legend = {
                enabled: false
            };

            xAxis = {
                endOnTick: false,
                gridLineWidth: 0,
                labels: {
                    formatter: function () {
                        return ticksMidText[this.value];
                    }
                },
                min: 0,
                max: g.CHROMOSOMES.totalLength,
                plotLines: plotLines,
                showLastLabel: true,
                startOnTick: false,
                tickPositions: ticksMidVals,
                title: {
                    enabled: true,
                    text: 'Marker'
                },
            };

            yAxis = {
                endOnTick: false,
                gridLineWidth: 0,
                labels: {
                    formatter: function () {
                        return ticksMidText[this.value];
                    }
                },
                min: 0,
                max: g.CHROMOSOMES.totalLength,
                plotLines: plotLines,
                showLastLabel: true,
                startOnTick: false,
                tickPositions: ticksMidVals,
                title: {
                    enabled: true,
                    text: 'Gene'
                },
            };

            clickFunction = function (event) {
                // dbg('clickFunction: ', this.geneID, this.proteinID, g.dataSetID, this.covar);
                selectGeneProtein(this.geneID, this.proteinID, g.dataSetID, this.covar);
            };

            tooltip = {
                followPointer: false,
                outside: true,
                formatter: function (a) {
                    return `LOD: ${this.point.lod}<br>Protein ID: ${this.point.proteinID}<br>Gene ID: ${this.point.geneID}<br>Symbol: ${this.point.geneSymbol}<br>Gene Location: ${this.point.geneChr.chromosome}:${this.point.genePos}<br>Marker ID: ${this.point.markerID}<br>Marker Location: ${this.point.markerChr.chromosome}:${this.point.markerPos}`;
                }
            };
        } else if (g.DATASETS[g.dataSetID].datatype === 'pheno') {
            // ["5_137006393","5",137.0064,"MEcyan","MEcyan","MEcyan",9.1161],[

            let seriesData = {};
            $.each(g.DATASETS[g.dataSetID].lodpeaks[covar], function(index, element) {
                // element[0] = marker_id
                let markerChr = g.CHROMOSOMES.chr[element[1]];
                let markerPos = element[2] * 1000000.0;

                // lookup the phenotype
                let phenotype = g.DATASETS[g.dataSetID].phenotypes[element[3]];

                if (!(phenotype.category in seriesData)) {
                    seriesData[phenotype.category] = [];
                }

                seriesData[phenotype.category].push({
                    x: markerChr.start + markerPos,
                    y: element[6],
                    lod: element[6],
                    phenoDataName: element[3],
                    phenoShortName: element[4],
                    phenoDescription: element[5],
                    markerPosOrig: element[2],
                    markerChr: markerChr,
                    markerPos: markerPos,
                    markerID: element[0],
                    covar: covar
                });
                minLOD = Math.min(minLOD, element[6]);
                maxLOD = Math.max(maxLOD, element[6]);
            });

            logDebug('minLOD=', minLOD, 'maxLOD=', maxLOD);

            minLOD = Math.floor(minLOD);
            maxLOD = Math.ceil(maxLOD);

            logDebug('ADJUSTED: minLOD=', minLOD, 'maxLOD=', maxLOD);

            $('#divLODPeaks').html(html);

            // dbg('seriesData=', seriesData);

            series = [];
            $.each(seriesData, function(index, element) {
                // dbg(index, element);
                series.push({
                    data: element,
                    name: index,
                    turboThreshold: 0,
                });
            });

            legend = {
                align: 'center',
                floating: false,
                layout: 'horizontal',
                verticalAlign: 'bottom',
            };

            xAxis = {
                endOnTick: false,
                gridLineWidth: 0,
                labels: {
                    formatter: function () {
                        return ticksMidText[this.value];
                    }
                },
                min: 0,
                max: g.CHROMOSOMES.totalLength,
                plotBands: plotBands,
                plotLines: plotLines,
                showLastLabel: true,
                startOnTick: false,
                tickPositions: ticksMidVals,
                title: {
                    enabled: true,
                    text: 'Marker Location'
                },
            };

            yAxis = {
                title: {
                    enabled: true,
                    text: 'LOD'
                },
                min: minLOD,
                max: maxLOD,
                tickWidth: 0,
                startOnTick: true,
                endOnTick: true,
                showLastLabel: true
            };

            clickFunction = function (event) {
                // dbg('clickFunction: ', this.phenoDataName, g.dataSetID, this.covar);
                selectPhenotype(this.phenoDataName, g.dataSetID, this.covar);
            };

            tooltip = {
                followPointer: false,
                outside: true,
                formatter: function () {
                    return `LOD: ${this.point.lod}<br>Phenotype: ${this.point.phenoShortName}<br>Category: ${this.series.name}<br><br>Marker ID: ${this.point.markerID}<br>Marker Location:${this.point.markerChr.chromosome}:${this.point.markerPos}`;
                }
            };

        } else {
            logError('WHAT TO DO????????????????????????????');
        }

        g.chartPeaks = Highcharts.chart('plotLodPeaks', {
            boost: {
                debug: {
                    showSkipSummary: true
                },
                enabled: true,
                useGPUTranslations: true,
            },
            chart: {
                type: 'scatter',
                zoomType: 'xy',
            },
            exporting: exporting,
            legend: legend,
            plotOptions: {
                scatter: {
                    marker: {
                        radius: 2,
                        states: {
                            hover: {
                                enabled: true,
                                lineColor: 'rgb(100,100,100)'
                            }
                        }
                    },
                    states: {
                        hover: {
                            marker: {
                                enabled: false
                            }
                        }
                    }
                },
                series: {
                    point: {
                        events: {
                            click: clickFunction,
                            mouseOver: function(evt) {
                                if(this.series.halo) {
                                    this.series.halo.attr({
                                       'class': 'highcharts-tracker'
                                    }).toFront();
                                }
                            }
                        }
                    }
                }
            },
            series: series,
            subtitle: {
                text: ''
            },
            title: {
                text: 'LOD Peaks'
            },
            tooltip: tooltip,
            xAxis: xAxis,
            yAxis: yAxis,
        });

        if ((g.DATASETS[g.dataSetID].datatype === 'mrna') ||
            (g.DATASETS[g.dataSetID].datatype === 'protein')) {
            // this hack is awful and needs to be fixed
            // this is the only way I could get the tooltips to appear
            // without manually moving the slider
            // I'm guessing it has something to do with canvas vs svg
            setTimeout(function () {
                // dbg('updatingPeaks 1');
                updatePeaks(maxLOD);
                setTimeout(function () {
                    // dbg('updatingPeaks 2');
                    updatePeaks(minLOD);
                }, 250);
            }, 250);
        }
    }

    function startTask() {
        g.runningTask = true;
        swal({
            allowEnterKey: false,
            allowEscapeKey: false,
            allowOutsideClick: false,
            buttonsStyling: false,
            cancelButtonClass: 'btn btn-danger',
            html: $('#ap').html(),
            showConfirmButton: false,
            showCancelButton: false,
            showCloseButton: true,
            title: 'Processing...',
            width: '20rem',
            onClose: function() {
                logDebug('onClose', this);
            }
        }).then((result) => {
            logDebug('then called');
            // result.dismiss can be 'cancel', 'overlay',
            // 'close', and 'timer'
            if (result.dismiss === 'close') {
                stopTask();
            }
        });
    }

    function stopTask() {
        g.runningTask = false;
        swal.close();
    }

    function downloadChromosomes(release, species, callback) {
        //let chromosomeURL = `/api/get/${g.PROTOCOL}//churchilllab.jax.org/ensimpl/api/chromosomes`;
        //let chromosomeURL = 'static/data/chromosomes.json';
        let chromosomeURL = 'http://ctlin0098.jax.org/ensimpl/api/chromosomes'
        let amp = '';

        if (release !== null) {
            chromosomeURL += `?release=${release}`;
            amp = '&';
        }

        if (species !== null) {
            chromosomeURL += `${amp}species=${species}`;
        }
        logInfo(chromosomeURL)
        downloadData(chromosomeURL, 'chromosomes', callback);
    }

    function downloadLODPeaks(dataSetID, callback) {
        //let lodPeaksURL = `/api/get/http://10.105.9.102/api/get/http://localhost:8001/lodpeaksall?dataset=${dataSetID}`;
        //let lodPeaksURL = 'static/data/' + dataSetID + '.json';
        let lodPeaksURL = _API_R_BASE + 'lodpeaksall?dataset=' + dataSetID;
        downloadData2(lodPeaksURL, 'LODPeaks', callback);
    }
// CCCC
    function downloadDataSets(callback) {
        //let dataSetsURL = '/api/get/http://10.105.9.102/api/get/http://localhost:8001/datasets';
        //let dataSetsURL = 'static/data/datasets.json'
    	let dataSetsURL = _API_R_BASE + 'datasets';
//    	let dataSetsURL = 'http://ctlin0098.jax.org/api-102/datasets'
    	logInfo(dataSetsURL)

        downloadData2(dataSetsURL, 'dataSets', callback);
    }

    function getTextEnd(text, dchart, start_position, end_position) {
        let G_ELEM = document.getElementById('getTheFontSize');
        let J_ELEM = $('#getTheFontSize');
        J_ELEM.text(text);

        let txtLengthPxls = G_ELEM.getComputedTextLength();

        let startPosPxls = dchart.xAxis[0].toPixels(start_position); // where the start is in pixels
        let endPosPxls = dchart.xAxis[0].toPixels(end_position); // where the end is in pixels

        let textEndPosCoord = dchart.xAxis[0].toValue(startPosPxls + txtLengthPxls + 5); // end value (megabases)

        let endPosCoord = Math.max(textEndPosCoord, end_position);
        let w = Math.ceil(endPosPxls - startPosPxls);

        //dbg(text, txtLengthPxls, start_position, end_position);
        //dbg(startPosPxls, endPosPxls, textEndPosCoord, endPosCoord, w);

            return {
                endPosCoord,
                w
            };
        }

        function configureGenes(chart, geneData) {
            let genes = [];
            let minCoord = 0;
            let maxCoord = 0;
            let shelves = [];

            $.each(geneData.matches, function(index, value) {
                if (value.symbol !== null) {
                    value.name = value.symbol;
                    value.start = value.position_start;

                    let te = getTextEnd(value.name, chart, value.position_start, value.position_end);

                    value.end = te.endPosCoord;
                    value.id = value.ensembl_id;

                    minCoord = Math.min(minCoord, value.start);
                    maxCoord = Math.max(maxCoord, value.end);

                    genes.push(value);
                }
            });

            shelves = addItems(shelves, genes, maxCoord - minCoord);

            return shelves;
        }



var hasPlotBand = false;

//catch mousemove event and have all 3 charts' crosshairs move along indicated values on x axis

function syncronizeCrossHairs(chart) {
    var container = $(chart.container),
        offset = container.offset(),
        x, y, isInside, report;

    container.mousemove(function(evt) {

        x = evt.clientX - chart.plotLeft - offset.left;
        y = evt.clientY - chart.plotTop - offset.top;
        var xAxis = chart.xAxis[0];
        var xVal = xAxis.toValue(x, true);

        //remove old plot line and draw new plot line (crosshair) for this chart
    var xAxis1 = g.chartSNPAssocSNPS.xAxis[0];
    /*
    var points1 = chart1.series[0].points;

    Highcharts.each(points1, function(point, i) {
      if (i + 1 < points1.length && point.x <= xVal && points1[i + 1].x > xVal) {
        //reset state
        point.setState();
        points1[i + 1].setState();

        if (xVal - point.x <= points1[i + 1].x - xVal) {
          chart1.tooltip.refresh(point);
          point.setState('hover');
        } else {
          chart1.tooltip.refresh(points1[i + 1]);
          points1[i + 1].setState('hover');
        }
      }
    });
    */

    xAxis1.removePlotLine("myPlotLineId");
    xAxis1.addPlotLine({
        value: chart.xAxis[0].translate(x, true),
        width: 1,
        color: 'red',
        //dashStyle: 'dash',
        id: "myPlotLineId",
        zIndex: 5
    });
    //remove old crosshair and draw new crosshair on chart2
    var xAxis2 = g.chartSNPAssocGenes.xAxis[0];
    xAxis2.removePlotLine("myPlotLineId");
    xAxis2.addPlotLine({
        value: chart.xAxis[0].translate(x, true),
        width: 1,
        color: 'red',
        //dashStyle: 'dash',
        id: "myPlotLineId",
        zIndex: 5
    });
/*
        Highcharts.each(g.chartSNPAssocGenes.series[0].points, function(point) {

            if (point.x < xVal && point.x2 > xVal) {
                //chart2.tooltip.refresh(point);

                //point.update({
                 // color: '#4879b2'
                //});

            } else {
                //point.update({
                //  color: '#fff'
                //});

            }
        });
    */
    /*
        var xAxis3 = chart3.xAxis[0];
        var points3 = chart3.series[0].points;
        Highcharts.each(points3, function(point, i) {
          if (i + 1 < points3.length && point.x <= xVal && points3[i + 1].x > xVal) {
            //reset state
            point.setState();
            points3[i + 1].setState();

            if (xVal - point.x <= points3[i + 1].x - xVal) {
              chart3.tooltip.refresh(point);
              point.setState('hover');
            } else {
              chart3.tooltip.refresh(points3[i + 1]);
              points3[i + 1].setState('hover');
            }
          }
        });

        xAxis3.removePlotLine("myPlotLineId");
        xAxis3.addPlotLine({
          value: chart.xAxis[0].translate(x, true),
          width: 1,
          color: 'red',
          //dashStyle: 'dash',
          id: "myPlotLineId"
        });
    */
    //if you have other charts that need to be syncronized - update their crosshair (plot line) in the same way in this function.
    });
}



        function exportSNPAssocData(id, snps, genes) {
            // TODO: export genes?
        let csvContent = `"id","position","alleles","csq","lod"\n`;

        $.each(snps, function(k, v) {
            let pos = v.x / 1000000.0;
            csvContent += `"${v.id}",${pos},"${v.alleles}","${v.csq}",${v.lod}\n`;
        });

        downloadCSV(csvContent, `${id}_SNPASSOC.csv`, 'text/csv;encoding:utf-8');
    }


    /**
     * Plot the SNP Association plot.
     * @param {Array} snpData - array of SNP information
     * @param {Array} geneData - array of gene objects
     */
    function plotSNPAssoc(snpData, geneData, id, chromosome, location, covar) {
        logDebug('plotSNPAssoc', chromosome, location, covar);

        let windowSize = 1000000;
        let minPos = Math.max(location * 1000000 - windowSize, 0);
        let maxPos = location * 1000000 + windowSize;
        let maxLODScore = null;
        let plotTitle = '';
        let currentID = '';

        if (g.DATASETS[g.dataSetID].datatype === 'mrna') {
            plotTitle = `${g.geneID} (${g.gene.gene[g.geneID].symbol})`;
            currentID = g.geneID;
        } else if (g.DATASETS[g.dataSetID].datatype === 'protein') {
            plotTitle = `${g.proteinID} (${g.gene.gene[g.geneID].symbol})`;
            currentID = g.proteinID;
        } else if (g.DATASETS[g.dataSetID].datatype === 'pheno') {
            plotTitle = g.phenotypeID;
            currentID = g.phenotypeID;
        } else {
            // TODO: handle error
            logError('MAJOR PROBLEM');
        }

        $('#snpWindowMenu').html(`
            <div class="col">
                <button id="snpShiftLeft" class="btn btn-secondary"><i class="fas fa-arrow-left"></i> Shift Plot Left</button>
            </div>
            <div class="col">
                <button id="snpShiftRight" class="btn btn-secondary float-right">Shift Plot Right <i class="fas fa-arrow-right"></i></button>
            </div>`);

        $('#snpShiftLeft').button().on('click', function(event) {
            generateSNPAssocPlot(id, chromosome, location - 1, covar);
        });

        $('#snpShiftRight').button().on('click', function(event) {
            generateSNPAssocPlot(id, chromosome, location + 1, covar);
        });

        snpData.sort(function(a, b) {
            return a.pos - b.pos;
        });

        let snps = [];
        let snpsCovar = [];

        if (snpData) {
            maxLODScore = snpData[0][10];

            if ((covar !== null) && (covar !== 'additive')) {
                maxLODScore = Math.max(maxLODScore, snpData[0][11]);
            }
        }

        let minn = Infinity;
        let maxx = -Infinity;

        /*
     0   "snp":"rs245710663",
     1   "chr":"1",
     2   "pos":14.5001,
     3   "alleles":"G|C",
     4   "sdp":64,
     5   "ensembl_gene":"",
     6   "csq":"intergenic_variant",
     7   "index":1,
     8   "interval":147,
     9   "on_map":false,
     10  "lod":0.0015

     IF covar:
     11  "lod_interactiveCovar":0.0029
        */

        $.each(snpData, function(index, element) {
            maxLODScore = Math.max(maxLODScore, element[10]);

            snps.push({
                alleles: element[3],
                csq: element[6],
                id: element[0],
                lod: element[10],
                x: element[2] * 1000000,
                y: 1.0 * element[10],
            });

            if ((covar !== null) && (covar !== 'additive')) {
                maxLODScore = Math.max(maxLODScore, element[11]);

                snpsCovar.push({
                    alleles: element[3],
                    csq: element[6],
                    id: element[0],
                    lod: element[11],
                    x: element[2] * 1000000,
                    y: 1.0 * element[11],
                });
            }

            minn = Math.min(minn, element[2] * 1000000.0);
            maxx = Math.max(maxx, element[2] * 1000000.0);

        });

        maxLODScore = Math.ceil(maxLODScore);

        logDebug('maxLODScore', maxLODScore);
        logDebug('minn', minn);
        logDebug('maxx', maxx);
        logDebug('minPos', minPos);
        logDebug('maxPos', maxPos);
        logDebug('#snps=', snps.length);
        logDebug('snps=', snps);
        logDebug('#snpsCovar=', snpsCovar.length);
        logDebug('snpsCovar=', snpsCovar);

        let seriesData = [{
            name: 'SNP',
            data: snps,
            turboThreshold: 0,
            color: '#ed3215',

            /*
            marker: {
                enabled: true,
                symbol: 'square',
                radius: 2.0,
            },
            */
            tooltip: {
                outside: true,
                useHTML: true,
                headerFormat: '<span class="strong">SNP</span><br>',
                pointFormatter: function() {
                    let csq = this.csq.split(',');
                    let csqs = csq.join('<br/>');
                    let pos = formatMbp(this.x / 100000);

                    return `<b>${this.id}</b><br/>
                         LOD: ${this.lod}<br/>
                         Position: ${pos}<br/>
                         Alleles: ${this.alleles}<br/>
                         CSQ: ${csqs}`;
                }
            },


            /* Set the point threshold for when a series should enter boost mode.
               Setting it to e.g. 2000 will cause the series to enter boost mode when there are 2000 or more points in the series.
               To disable boosting on the series, set the boostThreshold to 0. Setting it to 1 will force boosting.
               Requires modules/boost.js.
               Defaults to 5000.
            */
            boostThreshold: 1,
            //findNearestPointBy: 'xy',

        }];

        if ((covar !== null) && (covar !== 'additive')) {
            seriesData.push({
                name: 'SNP Covar',
                data: snpsCovar,
                turboThreshold: 0,
                color: '#7cb5ec',
                /*
                marker: {
                    enabled: true,
                    symbol: 'square',
                    radius: 2.0,
                },
                */
                tooltip: {
                    outside: true,
                    useHTML: true,
                    headerFormat: '<span class="strong">SNP Covariate</span><br>',
                    pointFormatter: function() {
                        let csq = this.csq.split(',');
                        let csqs = csq.join('<br/>');
                        let pos = formatMbp(this.x / 100000);

                        return `<b>${this.id}</b><br/>
                             LOD: ${this.lod}<br/>
                             Position: ${pos}<br/>
                             Alleles: ${this.alleles}<br/>
                             CSQ: ${csqs}`;
                    }
                },

                /* Set the point threshold for when a series should enter boost mode.
                   Setting it to e.g. 2000 will cause the series to enter boost mode when there are 2000 or more points in the series.
                   To disable boosting on the series, set the boostThreshold to 0. Setting it to 1 will force boosting.
                   Requires modules/boost.js.
                   Defaults to 5000.
                */
                boostThreshold: 1,
                //findNearestPointBy: 'xy',

            });
        }


        logDebug('seriesData=', seriesData);


        let exporting = {
            menuItemDefinitions: {
                downloadPNGSNP: {
                    onclick: function () {
                        let xAxis1 = g.chartSNPAssocSNPS.xAxis[0];
                        xAxis1.removePlotLine("myPlotLineId");
                        let xAxis2 = g.chartSNPAssocGenes.xAxis[0];
                        xAxis2.removePlotLine("myPlotLineId");
                        Highcharts.exportCharts([
                                g.chartSNPAssocSNPS, g.chartSNPAssocGenes
                            ],
                            {
                                filename: currentID + '_SNPASSOC'
                            });
                    },
                    text: 'Download PNG image'
                },
                downloadJPEGSNP: {
                    onclick: function () {
                        let xAxis1 = g.chartSNPAssocSNPS.xAxis[0];
                        xAxis1.removePlotLine("myPlotLineId");
                        let xAxis2 = g.chartSNPAssocGenes.xAxis[0];
                        xAxis2.removePlotLine("myPlotLineId");
                        Highcharts.exportCharts([
                                g.chartSNPAssocSNPS, g.chartSNPAssocGenes
                            ],
                            {
                                filename: currentID + '_SNPASSOC',
                                type: 'image/jpeg'
                            });
                    },
                    text: 'Download JPEG image'
                },
                downloadSVGSNP: {
                    onclick: function () {
                        let xAxis1 = g.chartSNPAssocSNPS.xAxis[0];
                        xAxis1.removePlotLine("myPlotLineId");
                        let xAxis2 = g.chartSNPAssocGenes.xAxis[0];
                        xAxis2.removePlotLine("myPlotLineId");
                        Highcharts.exportCharts([
                                g.chartSNPAssocSNPS, g.chartSNPAssocGenes
                            ],
                            {
                                filename: currentID + '_SNPASSOC',
                                type: 'image/svg+xml'
                            });
                    },
                    text: 'Download SVG vector image'
                },
                downloadPDFSNP: {
                    // THERE IS AN ERROR EXPORTING, NOT SURE WHY
                    onclick: function () {
                        let xAxis1 = g.chartSNPAssocSNPS.xAxis[0];
                        xAxis1.removePlotLine("myPlotLineId");
                        let xAxis2 = g.chartSNPAssocGenes.xAxis[0];
                        xAxis2.removePlotLine("myPlotLineId");
                        Highcharts.exportCharts([
                                g.chartSNPAssocSNPS, g.chartSNPAssocGenes
                            ],
                            {
                                filename: currentID + '_SNPASSOC',
                                type: 'application/pdf'
                            });
                    },
                    text: 'Download PDF document'
                },
                downloadCSVSNP: {
                    onclick: function () {
                        console.log(currentID);
                        exportSNPAssocData(currentID, snps, geneData);
                    },
                    text: 'Download CSV'
                }
            },
            buttons: {
                contextButton: {
                    menuItems: [
                        'downloadPNGSNP',
                        'downloadJPEGSNP',
                        'downloadSVGSNP',
                        'separator',
                        'downloadCSVSNP'
                    ]
                }
            },
            filename: currentID + '_SNPASSOC'
        };

        g.chartSNPAssocSNPS = Highcharts.chart({
            boost: {
                debug: {
                    showSkipSummary: false
                },
                enabled: true,
                useGPUTranslations: true,
                usePreallocated: true,
            },
            chart: {
                animation: false,
                type: 'scatter',
                marginLeft: 80,
                renderTo: 'plotSNPAssocSNPS',
                //panning: true,
                //panKey: 'shift',
                zoomType: 'x',
                //events: {
                //    load: function () {
                //        fitSNPGenes(this, geneData);
                //    }
                //}
            },

            title: {
                text: plotTitle,
            },

            xAxis: [{
                min: minPos,
                max: maxPos,
                events: {
                    afterSetExtremes: function() {
                        var xMin = this.chart.xAxis[0].min;
                        var xMax = this.chart.xAxis[0].max;

                        if ((g.chartSNPAssocGenes !== undefined) && (g.chartSNPAssocGenes !== null)) {

                            g.chartSNPAssocGenes.xAxis[0].setExtremes(xMin, xMax);
                        }
                        // anotherChart.xAxis[0].setExtremes(xMin, xMax);
                    }
                },
                type: 'linear',
            }],

            yAxis: {
                // enabling min and max DISABLES the tooltip
                //min: 0,
                max: maxLODScore,

                title: {
                    text: '',
                    lineWidth: 0, //get rid of the line
                    minorGridLineWidth: 0,
                    lineColor: 'transparent',
                },
            },

            legend: {
                enabled: true,
                useHTML: true,
                layout: 'horizontal',
                //align: 'middle',
                verticalAlign: 'top',
                //floating: false,
    labelFormatter: function () {
        return '<span style="color:' + this.color + '">' + this.name + '</span>';
    }

            },
            /*
            legend: {
                enabled: false
            },
            */

            series: seriesData,

            /*
            tooltip: {
                outside: true,
                useHTML: true,
                shared: false,
                followPointer: true,
                enabled: true,
                formatter: function () {
                    let text = '';
                    //logDebug('this.series.name=', this.series.name);
                    if (this.series.name === 'SNP') {
                        let csq = this.point.csq.split(',');
                        let csqs = csq.join('<br/>');
                        let pos = formatMbp(this.point.y / 100000);

                        text = `<b>${this.point.id}</b><br/>
                             LOD: ${this.point.lod}<br/>
                             Position: ${pos}<br/>
                             Alleles: ${this.point.alleles}<br/>
                             CSQ: ${csqs}`;
                    } else if (this.series.name === 'SNP Covar') {
                        let csq = this.point.csq.split(',');
                        let csqs = csq.join('<br/>');
                        let pos = formatMbp(this.point.y / 100000);

                        text = `<b>${this.point.id}</b><br/>
                             LOD: ${this.point.lod}<br/>
                             Position: ${pos}<br/>
                             Alleles: ${this.point.alleles}<br/>
                             CSQ: ${csqs}`;
                    } else {
                        text = `<b>${this.point.symbol}</b><br/>
                             Ensembl ID: ${this.point.ensembl_gene_id}<br/>
                             Name: ${this.point.name}<br/>
                             Position: ${this.point.match_value} (${this.point.strand})`;

                    }
                    // dbg(text);
                    return text;
                }
            },
            */

            exporting: exporting,

            plotOptions: {
                scatter: {
                    marker: {
                        radius: 1.0
                    }
                }
            }
            /*

            plotOptions: {
                spline: {
                    marker: {
                        enabled: false
                    }
                },
            }
            */



        }, function(chart) {
            syncronizeCrossHairs(chart);
        });


    // dbg('geneData = ', geneData);
    let tempGenes = configureGenes(g.chartSNPAssocSNPS, geneData);
    // dbg('tempGenes = ', tempGenes);
    let newGenes = [];
    let cats  = {};

    $.each(tempGenes, function(key, value) {
        let slot = key;
        cats[key] = key;

        $.each(value.elements, function(i, element) {
            element.x = element.start;
            element.x2 = element.position_end;
            element.y = slot ;
            newGenes.push(element);
        });

    });

    var newCats = [];
    $.each(cats, function(key, value) {
        newCats.push(+value);
    });
    newCats = newCats.sort(function(a, b){return a-b});

    // this is a hack to add one more category so the last one is
    // always displayed
    logDebug('newCats=', newCats);
    // always show 1 extra category because the text is below the bar
    let maxYCategories = newCats.length + 1;

    //newCats.push(newCats.length);
    //newCats.push(newCats.length);
    //newCats.push(newCats.length);


    g.GENESREDRAWENABLED = true;
    g.chartSNPAssocGenes = Highcharts.chart({
        chart: {
            animation: false,
            renderTo: 'plotSNPAssocGenes',
            type: 'xrange',
            zoomType: 'x',
            marginLeft: 80,
            marginTop: 0,
            height: 100,  // this will be overridden on load
            events: {

                load: function() {

                                            let chart = this;

            var categories =chart.yAxis[0].categories.length,
                plotHeight = (categories + 1) * 40,  // 40 is for the space between the bars
                chartHeight = plotHeight + (chart.chartHeight - chart.plotHeight);
                console.log(categories);

            if (chartHeight > this.chartHeight) {
                this.setSize(null, chartHeight, false)
            }
            console.log(chartHeight, this.chartHeight);
                },

                render: function() {
                    logDebug('R E N D E R');
                    if (this.series.length !== 1) {
                        return;
                    }
                    if (g.GENESREDRAWENABLED) {
                        let points = this.series[0].points,
                            chart = this,
                            //dataLabelWidth,
                            height;

                        g.GENESREDRAWENABLED = false;

                        Highcharts.each(points, function(point) {
                            height = point.shapeArgs.height;

                            if ((point.dataLabel.x < point.shapeArgs.x) || (point.dataLabel.y < point.shapeArgs.y) ||
                                (point.shapeArgs.x < 0)) {
                                point.dataLabel.hide();
                            } else {
                                point.dataLabel.show();
                            }
                        });

                        chart.series[0].isDirty = true;
                        chart.redraw();

                        g.GENESREDRAWENABLED = true;
                    }

                },

            },
        },
        exporting: {
            enabled: false,
        },
        credits: {
            enabled: false
        },

        title: {
            text: null,
        },

        legend: {
            enabled: false
        },
        xAxis: {
            min: minPos,
            max: maxPos,
            events: {
                afterSetExtremes: function() {
                    var xMin = this.chart.xAxis[0].min;
                    var xMax = this.chart.xAxis[0].max;

                    g.chartSNPAssocSNPS.xAxis[0].setExtremes(xMin, xMax);
                    //chart3.xAxis[0].setExtremes(xMin, xMax);
                }
            },
            //type: 'linear',
            //labels: {
            //      format: '{value:%b %e}'
            //},
            //maxPadding: 0.06,
            scrollbar: {
                enabled: true,
                showFull: false
            }
        },
        yAxis: {
            title: {
                text: null
            },
            lineWidth: 0,
            minorGridLineWidth: 0,
            lineColor: 'transparent',

            gridLineWidth: 0,
            //lineWidth: 1,

            /*

            scrollbar: {
                enabled: true,
                showFull: false
            },
            */

            labels: {
                enabled: false
            },

            minorTickLength: 0,
            tickLength: 0,
            reversed: true,
            //min: -12,
            max: maxYCategories,
            //categories: [2, 1, 0, -1, -2, -3, -4, -5, -6, -7, -8, -9, -10, -11, -12],
            categories: newCats,

        },


        plotOptions: {
            series: {
                //pointRange: 2,
                //pointWidth: 10,
                minPointLength: 1,
    pointWidth: 10,
    pointPadding: 0,
    groupPadding: 0,

            },
            xrange: {
                //pointPlacement: "on",

                borderWidth: 1,
                borderColor: 'black',
                colorByPoint: true,
                colors: ['#7cb5ec'],
                dataLabels: {
                    style: {
                        color: 'white'
                    }
                }
            },
        },

        tooltip: {
            useHTML: true,
            shared: false,
            followPointer: true,
            enabled: true,
            outside: true,
            formatter: function () {
                let text = '';
                // dbg('this.series.name=', this.series.name);
                if (this.series.name === 'SNP') {
                    let csq = this.point.csq.split(',');
                    let csqs = csq.join('<br/>');
                    let pos = formatMbp(this.point.y / 100000);

                    text = `<b>${this.point.id}</b><br/>
                         LOD: ${this.point.lod}<br/>
                         Position: ${pos}<br/>
                         Alleles: ${this.point.alleles}<br/>
                         CSQ: ${csqs}`;
                } else if (this.series.name === 'SNP Covar') {
                    let csq = this.point.csq.split(',');
                    let csqs = csq.join('<br/>');
                    let pos = formatMbp(this.point.y / 100000);

                    text = `<b>${this.point.id}</b><br/>
                         LOD: ${this.point.lod}<br/>
                         Position: ${pos}<br/>
                         Alleles: ${this.point.alleles}<br/>
                         CSQ: ${csqs}`;
                } else {
                    text = `<b>${this.point.symbol}</b><br/>
                         Ensembl ID: ${this.point.ensembl_gene_id}<br/>
                         Name: ${this.point.name}<br/>
                         Position: ${this.point.match_value} (${this.point.strand})`;

                }
                // dbg(text);
                return text;
            }
        },

        series: [{
            name: "seriesGenes",
            data: newGenes,
            //dataLabels: {
            //    verticalAlign: "top",
            //    y: 10
            //},
            type: "xrange",
            //marginLeft: 10,
            borderRadius: 0,

            dataLabels: {
                align: 'left',
                allowOverlap: true,
                color: 'black',
                defer: false,
                enabled: true,
                formatter: function() {
                    return this.point.symbol;
                },
                //inside: true,
                y:15,
                padding: 0,
                style: {
                    textOutline: false
                },
            },

        }]
    }, function(chart) {
        syncronizeCrossHairs(chart);
    });





    }

    /**
     * Start downloading snp
     */
    function generateSNPAssocPlot(id, chromosome, location, covar) {
        // TODO: windowSize needs to be figured out
        let windowSize = 1000000;
        let newLocation = Math.round(location * 1000000.0);
        let minPos = Math.max(newLocation - windowSize, 0);
        let maxPos = newLocation + windowSize;

//        let snpAssocURL = `http://10.105.9.102/api/get/http://localhost:8001/snpassoc?dataset=${g.dataSetID}&id=${id}&chrom=${chromosome}&location=${newLocation}&window_size=${windowSize}`;
//        let genesURL = `${g.PROTOCOL}//churchilllab.jax.org/ensimpl/api/search?species=${g.SPECIES_ID}&term=${chromosome}:${minPos}-${maxPos}`;

        let snpAssocURL = _API_R_BASE + 'snpassoc?dataset=' + g.dataSetID
        		+ '&id=' + id + '&chrom=' + chromosome + '&location=' + newLocation + '&window_size=' + windowSize;
        let genesURL = 'http://ctlin0098.jax.org/ensimpl/api/search?species='
        		+ g.SPECIES_ID + '&term=' + chromosome + ':' + minPos + '-' + maxPos;

        if (g.ENSEMBL_RELEASE != null) {
            genesURL += '&release=' + g.ENSEMBL_RELEASE;
        }

        if (covar !== 'additive') {
            snpAssocURL += '&intcovar=' + covar;
        }


        snpAssocURL += '&cores=5';


        let submitData = {
            urls:[{
                url_id: 'snpAssoc',
                url: snpAssocURL
            }, {
                url_id: 'genes',
                url: genesURL,
                is_call_outside: true
            }]};

        startTask();

        // reset the chart and clear it
        g.chartSNPAssocSNPS = null;
        g.chartSNPAssocGenes = null;
        $('#plotSNPAssocSNPS').html('');
        $('#plotSNPAssocGenes').html('');
        $('#snpWindowMenu').html('');

        var q = d3.queue();
        for (var i=0; i<submitData.urls.length; i++) {
            var url = submitData.urls[i];
            if ( url.is_call_outside ) {
                q.defer(downloadData, url.url, url.url_id);
            } else {
                q.defer(downloadData2, url.url, url.url_id);
            }
        }

        var group_id = newCallGroup(submitData.urls);
        q.awaitAll(function(error, responseList ) {
            updateGroupResponse(group_id, error, responseList);
        	updateSNPAssocData(group_id, id, chromosome, location, covar);
        	g.runningTask = true;
            logDebug('group_id=', group_id);
        });

//        $.ajax({
//            type: 'POST',
//            url: '/api/submit',
//            contentType: 'application/json',
//            data: JSON.stringify(submitData),
//            retries: 3,
//            retryInterval: 1000,
//            success: function(data, status, request) {
//                logDebug('data=', data);
//                updateSNPAssocData(data.group_id, id, chromosome, location, covar);
//            },
//            error: function(jqXHR, textStatus, errorThrown) {
//                showErrorMessage(errorThrown, textStatus);
//            }
//        });
    }

    /**
     * Update the status of the downloading effect data.
     *
     * @param {string} groupID - the group identifier of the task
     */
    function updateSNPAssocData(groupID, id, chromosome, location, covar) {
        // send GET request to status URL
        logDebug('updateSNPAssocData: ', groupID);

        if (g.runningTask) {
            let statusURL = 'http://ctlin0098.jax.org/';
            $.ajax({
                type: 'GET',
                url: statusURL,
                retries: 3,
                retryInterval: 1000,
                success: function (dataIn, status, request) {
                    var data = getCallGroupResponse(groupID)
                    logDebug('DATA=======', data);
                    if (data.status === 'DONE') {
                        if ('error' in data) {
                            // MAJOR ERROR
                            let message = `Unfortunately, there was a problem contacting the server.  Please try again.`;
                            stopTask();
                            showErrorMessage(message, null);
                        } else if (data.number_tasks_errors !== 0) {
                            let message = `Unfortunately, we encountered an error.  Please try again.`;
                            let errorMessages = '';
                            $.each(data.response_data, function (key, value) {
                                if ('error' in value) {
                                    errorMessages += (`<strong>${key}:</strong> ${value.error}<br>`);
                                }
                            });

                            stopTask();
                            showErrorMessage(message, errorMessages);
                        } else if (data.number_tasks_errors === 0) {
                            // check to make sure the status codes are good
                            let errorMessages = '';
                            $.each(data.response_data, function (key, value) {
                                if (value.status_code !== 200) {
                                    errorMessages += (`<strong>${key}:</strong> ${value.response.error}<br>`);
                                }
                            });

                            if (errorMessages !== '') {
                                let message = `Unfortunately, there was a problem calculating the SNP Association Plot.`;
                                stopTask();
                                showErrorMessage(message, errorMessages);
                            } else {
                                // show result, there will be 2 datasets to get
                                plotSNPAssoc(data.response_data.snpAssoc.response.result,
                                             data.response_data.genes.response.result,
                                             id, chromosome, location, covar);
                                stopTask();
                            }
                        }
                    } else {
                        // rerun in 1 seconds
                        logDebug('Not done, keep checking...');
                        setTimeout(function () {
                            updateSNPAssocData(groupID, id, chromosome, location, covar);
                        }, 1000);  // TODO: change to 1000 (1 second)
                    }
                }
            });
        } else {
            // TODO: cleanup
            logDebug('canceling');
            let cancelURL = `/api/cancel/${groupID}`;
            $.getJSON(cancelURL, function (data) {
                g.chartSNPAssocSNPS = null;
                g.chartSNPAssocGenes = null;
                $('#plotSNPAssocSNPS').html('');
                $('#plotSNPAssocGenes').html('');
                $('#snpWindowMenu').html('');
            });
        }
    }

    function exportEffectData(id, effectData, lodData, covar, category) {
        let csvContent = `"id","chromosome","position","A/J","C57BL/6J","129S1/SvImJ","NOD/ShiLtJ","NZO/H1LtJ","CAST/EiJ","PWK/PhJ","WSB/EiJ","lod"\n`;

        $.each(effectData, function(k, v) {
            let line = `"${v[0]}","${v[1]}",${v[2]},${v[3]},${v[4]},${v[5]},${v[6]},${v[7]},${v[8]},${v[9]},${v[10]},`;
            line += lodData[k].y;
            csvContent += `${line}\n`;
        });

        if (category !== null) {
            downloadCSV(csvContent, `${id}_EFFECT_${covar}_${category}.csv`, 'text/csv;encoding:utf-8');
        } else {
            downloadCSV(csvContent, `${id}_EFFECT.csv`, 'text/csv;encoding:utf-8');
        }
    }

    /**
     * Plot the effect data.
     * @param {Object} effectData - the effect data
     */
    function plotEffectData(effectData, lodData, chromosome, covar) {
        logDebug('plotEffectData() ', effectData, lodData, covar);

        if (covar === 'additive') {
            plotEffectChart(effectData[covar], null, covar, null, chromosome);
        } else {
            $.each(effectData, function(key, value) {
                // key = category value, i.e for sex would be M,F
                plotEffectChart(value, lodData[key], covar, key, chromosome);
            });
        }
    }

    function plotEffectChart(effectData, lodData, covar, category, chromosome) {
        logDebug('plotEffectChart() ', effectData, lodData, covar, category);

        let strainInfo = {};
        strainInfo['A'] = {
            color: '#f9c922',
            name: 'A/J',
            data: []
        };
        strainInfo['B'] = {
            color: '#888888',
            name: 'C57BL/6J',
            data: []
        };
        strainInfo['C'] = {
            color: '#F08080',
            name: '129S1/SvImJ',
            data: []
        };
        strainInfo['D'] = {
            color: '#0064C9',
            name: 'NOD/ShiLtJ',
            data: []
        };
        strainInfo['E'] = {
            color: '#7FDBFF',
            name: 'NZO/H1LtJ',
            data: []
        };
        strainInfo['F'] = {
            color: '#2ECC40',
            name: 'CAST/EiJ',
            data: []
        };
        strainInfo['G'] = {
            color: '#FF4136',
            name: 'PWK/PhJ',
            data: []
        };
        strainInfo['H'] = {
            color: '#B10DC9',
            name: 'WSB/EiJ',
            data: []
        };

        let renderTo = '';
        let isCovar = false;

        if (covar === 'additive') {
            renderTo = 'plotEffect';
            // get the data from the lod chart, but filter for just the chromosome
            lodData = g.LODChartData[covar];
        } else {
            isCovar = true;
            renderTo = 'plotEffect_' + category;
        }

        $('#allEffectPlots').append(`<div class="row"><div class="col">
                                            <div id="${renderTo}"></div>
                                        </div></div>`);

        logDebug('covar=', covar);
        logDebug('isCovar=', isCovar);
        logDebug('renderTo=', renderTo);

        let allEffectValues = [];

        $.each(effectData, function(index, element) {
            //{"A":-0.1224,..."H":1.45,"chr":"4","id":"4_156339889","pos":156.3399}
            //id,chrom,pos,A-H
            let position = element[2] * 1000000;
            strainInfo['A'].data.push([position, element[3]]);
            allEffectValues.push(element[3]);
            strainInfo['B'].data.push([position, element[4]]);
            allEffectValues.push(element[4]);
            strainInfo['C'].data.push([position, element[5]]);
            allEffectValues.push(element[5]);
            strainInfo['D'].data.push([position, element[6]]);
            allEffectValues.push(element[6]);
            strainInfo['E'].data.push([position, element[7]]);
            allEffectValues.push(element[7]);
            strainInfo['F'].data.push([position, element[8]]);
            allEffectValues.push(element[8]);
            strainInfo['G'].data.push([position, element[9]]);
            allEffectValues.push(element[9]);
            strainInfo['H'].data.push([position, element[10]]);
            allEffectValues.push(element[10]);
        });

        let effectMinValue = Math.floor(Math.min.apply(null, allEffectValues));
        let effectMaxValue = Math.ceil(Math.max.apply(null, allEffectValues));

        logDebug('effectMinValue=', effectMinValue);
        logDebug('effectMaxValue=', effectMaxValue);

        /*
        if ((effectMaxValue > 30.0) || (effectMinValue < -30.0)) {
            displayError('Effect Plot Error', 'Effect values appear to be invalid, ' + effectMinValue + ' to ' + effectMaxValue);
            return;
        }
        */

        logDebug('strainInfo=', strainInfo);

        let xAxisTickNum = Math.floor(g.CHROMOSOMES.chr[chromosome].length / 20000000);
        let xAxisTickVals = [];
        let xAxisTickText = [];
        let series = [];

        for (let i = 1; i <= xAxisTickNum; i++) {
            xAxisTickVals.push(20000000 * i);
            xAxisTickVals.push((20 * i) + 'Mb');
        }

        $.each(strainInfo, function(key, value) {
            let s = {
                animation: 0,
                color: value.color,
                data: value.data,
                lineWidth: 1,
                marker: {
                    symbol: 'circle',
                },
                name: value.name,
                showInLegend: true,
                stack: 0,
                tooltip: {
                    shared: false,
                    headerFormat:'<b>{series.name}</b>',
                    pointFormatter: function() {
                        return '<br>Effect: ' + this.y + '<br>Position: ' +  (this.x/1000000);
                    }
                },
                turboThreshold: 0,
                type: 'line',
                yAxis:'axisEffect',
            };
            series.push(s);
        });

        let plotTitle = '';
        let currentID = '';

        if (g.DATASETS[g.dataSetID].datatype === 'mrna') {
            plotTitle = `${g.geneID} (${g.gene.gene[g.geneID].symbol})`;
            currentID = g.geneID;
        } else if (g.DATASETS[g.dataSetID].datatype === 'protein') {
            plotTitle = `${g.proteinID} (${g.gene.gene[g.geneID].symbol})`;
            currentID = g.proteinID;
        } else if (g.DATASETS[g.dataSetID].datatype === 'pheno') {
            plotTitle = g.phenotypeID;
            currentID = g.phenotypeID;
        } else {
            // TODO: handle error
            logError('MAJOR PROBLEM');
        }

        let newLodData = [];
        let lodMinValue = Infinity;
        let lodMaxValue = -Infinity;

        $.each(lodData, function(index, element) {
            //
            // element = {"[id]","[chr]",[position],[data]}
            //
            if ((element[1] in g.CHROMOSOMES.chr) && (element[1] === chromosome)) {

                let chr = g.CHROMOSOMES.chr[element[1]];
                let x = element[2] * 1000000.0;//Math.round(chr.start + (element[2] * 1000000.0));
                let y = element[3];

                let d = {
                    x: x,
                    y: y,
                    id: element[0],
                    text: element[3],
                    chr: chr.chromosome,
                    chrPos: element[2],
                    covar: covar
                };
                newLodData.push(d);
                lodMinValue = Math.min(element.y, lodMinValue);
                lodMaxValue = Math.max(element.y, lodMaxValue);
            }
        });

        series.push({
            animation: false,
            color: '#000000',
            data: newLodData,
            lineWidth: 1,
            name: 'LOD',
            showInLegend: false,
            stack: 0,
            tooltip: {
                shared: false,
                headerFormat: '',
                pointFormatter: function() {
                    let pos = this.x/1000000;
                    return `<br>LOD: ${this.y}<br>Position: ${pos}`;
                }
            },
            turboThreshold: 1000000, // TODO: what should this value be
            yAxis: 'axisLOD',
        });

        let lodTickVals = null;
        // round up to nearest 10, 11->20, 7->10, 88->90
        let lodMaxTick = Math.ceil(lodMaxValue / 10) * 10;

        // determine ticks if > 10, otherwise let the chart do it
        if (lodMaxValue > 10) {
            let lodMaxTick = lodMaxValue + 1;
            let maxNumTicks = Math.min(4, Math.ceil(lodMaxTick / 10) * 10);
            lodTickVals = [];

            let step = Math.ceil((lodMaxValue/maxNumTicks) / 10) * 10;
            for (let i = 0; i < lodMaxValue; i += step) {
                lodTickVals.push(i);
            }
            lodTickVals.push(lodMaxTick);
        }

        let exporting = appendExportButton('Download CSV', function() {
            exportEffectData(currentID, effectData, newLodData, covar, category);
        });

        exporting.filename = currentID + '_EFFECT';

        if (isCovar) {
            exporting.filename = currentID + '_EFFECT_' + category;
            plotTitle += (' (' + category + ')');
        }

        g.chartEffect[category] = Highcharts.chart({
            boost: {
                useGPUTranslations: true
            },
            chart: {
                renderTo: renderTo,
                zoomType: 'x'
            },
            exporting: exporting,
            legend: {
                align: 'right',
                backgroundColor: (Highcharts.theme && Highcharts.theme.legendBackgroundColor) || '#FFFFFF',
                floating: false,
                layout: 'vertical',
                verticalAlign: 'top',
                y: 100,
            },
            responsive: {
                rules: [{
                    condition: {
                        maxWidth: 400
                    },
                    chartOptions: {
                        legend: {
                            align: 'center',
                            layout: 'horizontal',
                            verticalAlign: 'top',
                            y: 0,
                        },
                    }
                }]
            },
            series: series,
            subtitle: {
                text: 'Chromosome ' + chromosome,
                verticalAlign: 'bottom',
            },
            title: {
                text: plotTitle,
            },
            tooltip: {
                outside: true,
            },
            xAxis: {
                crosshair: true,
                gridLineWidth: 1,
                lineWidth: 0.5,
            },
            yAxis: [{
                height: '70%',
                id: 'axisEffect',
                labels: {
                    formatter: function() {
                        if (this.isFirst) {
                            return '';
                        }
                        return this.value;
                    }
                },
                maxPadding: 0,
                offset: 0,
                title: {
                    text: 'EFFECT'
                },
            }, {
                height: '30%',
                id: 'axisLOD',
                labels: {
                    formatter: function() {
                        if (this.isLast) {
                            return '';
                        }
                        return this.value;
                    }
                },
                maxPadding: 0,
                offset: 0,
                plotLines: [{
                    value: lodMaxTick,
                    color: 'black',
                    zIndex: 5,
                    width: 2
                }],
                tickPositions: lodTickVals,
                title: {
                    text: 'LOD'
                },
                top: '70%',
            }],
        });
    }

    function exportMediationData(id, data) {
        let csvContent = `"gene_id","symbol","chromosome","position","lod"\n`;
        let protein = false;
        if (g.DATASETS[g.dataSetID].datatype === 'mrna') {
            protein = false;
        } else if (g.DATASETS[g.dataSetID].datatype === 'protein') {
            protein = true;
            csvContent = '"protein_id",' + csvContent;
        }

        $.each(data, function(k, v) {
            let line = `"${v.gene_id}","${v.symbol}","${v.chr}",${v.pos},${v.LOD}`;
            if (protein) {
                line = `"${v.protein_id}",` + line;
            }
            csvContent += `${line}\n`;
        });

        downloadCSV(csvContent, `${id}_MEDIATION.csv`, 'text/csv;encoding:utf-8');
    }


    /**
     * Plot mediation data
     * @param {Object} mediationData - mediation data
     */
    function plotMediation(mediationData, dsMediateAgainst) {
        let newData = [];

        if (g.DATASETS[dsMediateAgainst].datatype === 'mrna') {
            // ["ENSMUSG00000000001", "Gnai3", "3", 108.1267, 140.1666]
            $.each(mediationData, function(index, element) {
                if (element[2] in g.CHROMOSOMES.chr) {
                    let chr = g.CHROMOSOMES.chr[element[2]];
                    element.x = chr.start + (element[3] * 1000000.0);
                    element.y = element[4];

                    newData.push({
                        chr: element[2],
                        gene_id: element[0],
                        LOD: element[4],
                        pos: element[3],
                        symbol: element[1],
                        x: chr.start + (element[3] * 1000000.0),
                        y: element[4]
                    });
                } else {
                    //logDebug(element);
                }
            });

        } else if (g.DATASETS[dsMediateAgainst].datatype === 'protein') {
            // ["ENSMUSP00000000001", "ENSMUSG00000000001", "Gnai3", "3", 108.126713, 7.1102046955]
            $.each(mediationData, function(index, element) {
                if (element[3] in g.CHROMOSOMES.chr) {
                    let chr = g.CHROMOSOMES.chr[element[3]];
                    element.x = chr.start + (element[4] * 1000000.0);
                    element.y = element[5];

                    newData.push({
                        chr: element[3],
                        gene_id: element[1],
                        protein_id: element[0],
                        LOD: element[5],
                        pos: element[4],
                        symbol: element[2],
                        x: chr.start + (element[4] * 1000000.0),
                        y: element[5]
                    });
                } else {
                    //logDebug(element);
                }
            });
        } else {
            // TODO: handle error
            logError('MAJOR PROBLEM');
        }

        let axisTickVals = [];
        let axisTickText = [];

        // rectangles to display
        let dataChromosome = {};
        let chromosomeAxisVals = [];
        let chromosomeAxisText = {};
        let chromosomeBands = [];

        $.each(g.CHROMOSOMES.idx, function(index, element) {

            axisTickVals.push(element.mid);
            axisTickText.push(element.chromosome);

            dataChromosome[element.chromosome] = {};
            dataChromosome[element.chromosome].data = [];

            chromosomeAxisVals.push(element.mid);
            chromosomeAxisText[element.mid] = element.chromosome;

            // TODO: style this
            let fillColor = '#eeeeee';
            if ((index % 2) === 1) {
                chromosomeBands.push({
                    color: fillColor,
                    from: element.start,
                    to: element.end
                });
            }
        });

        let plotTitle = '';
        let currentID = '';

        if (g.DATASETS[g.dataSetID].datatype === 'mrna') {
            plotTitle = `${g.geneID} (${g.gene.gene[g.geneID].symbol})`;
            currentID = g.geneID;
        } else if (g.DATASETS[g.dataSetID].datatype === 'protein') {
            plotTitle = `${g.proteinID} (${g.gene.gene[g.geneID].symbol})`;
            currentID = g.proteinID;
        } else if (g.DATASETS[g.dataSetID].datatype === 'pheno') {
            plotTitle = g.phenotypeID;
            currentID = g.phenotypeID;
        } else {
            // TODO: handle error
            logError('MAJOR PROBLEM');
        }

        let exporting = appendExportButton('Download CSV', function() {
            exportMediationData(currentID, newData);
        });
        exporting.filename = currentID + '_MEDIATION';

        g.chartMediation = Highcharts.chart({
            chart: {
                renderTo: 'plotMediation',
                zoomType: 'x',
            },
            exporting: exporting,
            plotOptions: {
                scatter: {
                    marker: {
                        radius: 5,
                        states: {
                            hover: {
                                enabled: true,
                                lineColor: 'rgb(100,100,100)'
                            }
                        }
                    },
                    states: {
                        hover: {
                            marker: {
                                enabled: false
                            }
                        }
                    },
                }
            },
            series: [{
                data: newData,
                downsample: {
                    threshold: 0 // 0 disables downsampling
                },
                marker: {
                      radius: 2
                },
                showInLegend: false,
                turboThreshold: 0,
                type: 'scatter',
            }],
            title: {
                text: plotTitle,
            },
            tooltip: {
                useHTML: true,
                formatter: function () {
                    let p = this.point;
                    if (p.protein_id) {
                        return `<b>${p.protein_id}</b><br/>Symbol: ${p.symbol}<br/>Chromosome: ${p.chr}<br/>Location: ${p.pos}<br/>LOD: ${p.LOD}`;

                    } else {
                        return `<b>${p.gene_id}</b><br/>Symbol: ${p.symbol}<br/>Chromosome: ${p.chr}<br/>Location: ${p.pos}<br/>LOD: ${p.LOD}`;
                    }
                },
                outside: true,
            },
            xAxis: {
                gridLineWidth: 0,
                labels: {
                    formatter: function () {
                        return chromosomeAxisText[this.value];
                    }
                },
                opposite: true,
                plotBands: chromosomeBands,
                tickPositions: chromosomeAxisVals,
            },
            yAxis: {
                title: {
                    text: 'LOD'
                }
            },
        });
    }

    /**
     * Start downloading mediation data.
     *
     */
    function generateMediationPlot(id, markerID) {
        let mediateAgainst = $('#dsMediateAgainstID').val();
//        let mediateURL = `http://10.105.9.102/api/get/http://localhost:8001/mediate?dataset=${g.dataSetID}&id=${id}&marker_id=${markerID}&dataset_mediate=${mediateAgainst}`;
        let mediateURL = _API_R_BASE + 'mediate?dataset=' + g.dataSetID + '&id=' + id
        	+ '&marker_id=' + markerID + '&dataset_mediate=' + mediateAgainst;

        let submitData = {
            urls:[{
                url_id: 'mediate',
                url: mediateURL
            }]};

        startTask();

        // reset the chart and clear it
        g.chartMediation = null;
        $('#plotMediation').html('');

        var q = d3.queue();
        for (var i=0; i<submitData.urls.length; i++) {
            var url = submitData.urls[i];
            q.defer(downloadData2, url.url, url.url_id);
        }

        var group_id = newCallGroup(submitData.urls);
        q.awaitAll(function(error, responseList ) {
            updateGroupResponse(group_id, error, responseList);
        	updateMediationData(group_id, mediateAgainst);
        	g.runningTask = true;
            logDebug('group_id=', group_id);
        });

//        $.ajax({
//            type: 'POST',
//            url: '/api/submit',
//            contentType: 'application/json',
//            data: JSON.stringify(submitData),
//            retries: 3,
//            retryInterval: 1000,
//            success: function(data, status, request) {
//                logInfo('Mediation');
//                logDebug('data = ', data);
//                updateMediationData(data.group_id, mediateAgainst);
//            },
//            error: function(jqXHR, textStatus, errorThrown) {
//                showErrorMessage(errorThrown, textStatus);
//            }
//        });
    }

    /**
     * Update the status of the downloading effect data.
     *
     * @param {string} groupID - the group identifier of the task
     */
    function updateMediationData(groupID, dsMediateAgainst) {
        // send GET request to status URL
        logDebug('updateMediationData{} ', groupID);

        if (g.runningTask) {
		    let statusURL = 'http://ctlin0098.jax.org/';
            $.ajax({
                type: 'GET',
                url: statusURL,
                retries: 3,
                retryInterval: 1000,
                success: function (dataIn, status, request) {
                    var data = getCallGroupResponse(groupID)
                    logDebug('mediation data = ', data);

                    if (data.status === 'DONE') {
                        if ('error' in data) {
                            // MAJOR ERROR
                            let message = `Unfortunately, there was a problem contacting the server.  Please try again.`;
                            stopTask();
                            showErrorMessage(message, null);
                        } else if (data.number_tasks_errors !== 0) {
                            let message = `Unfortunately, we encountered an error.  Please try again.`;
                            let errorMessages = '';
                            $.each(data.response_data, function (key, value) {
                                if ('error' in value) {
                                    errorMessages += (`<strong>${key}:</strong> ${value.error}<br>`);
                                }
                            });

                            stopTask();
                            showErrorMessage(message, errorMessages);
                        } else if (data.number_tasks_errors === 0) {
                            // check to make sure the status codes are good
                            let errorMessages = '';
                            $.each(data.response_data, function (key, value) {
                                if (value.status_code !== 200) {
                                    errorMessages += (`<strong>${key}:</strong> ${value.response.error}<br>`);
                                }
                            });

                            if (errorMessages !== '') {
                                let message = `Unfortunately, there was a problem performing the Mediation Analysis.`;
                                stopTask();
                                showErrorMessage(message, errorMessages);
                            } else {
                                // show result, there will be 1 datasets to get
                                logInfo('mediation');
                                plotMediation(data.response_data.mediate.response.result, dsMediateAgainst);
                                stopTask();
                            }
                        }
                    } else {
                        // rerun in 1 seconds
                        logDebug('Mediation not done, keep checking...');
                        setTimeout(function () {
                            updateMediationData(groupID, dsMediateAgainst);
                        }, 1000);  // TODO: change to 1000 (1 second)
                    }
                }
            });
        } else {
            // TODO: cleanup
            logDebug('canceling');
            let cancelURL = `/api/cancel/${groupID}`;
            $.getJSON(cancelURL, function (data) {
                logDebug(data);
                g.chartMediation = null;
                $('#plotMediation').html('');
            });
        }
    }

    /**
     * Start downloading effect data.
     *
     */
    function generateEffectPlot(id, chromosome, covar) {
//        let effectURL = `http://10.105.9.102/api/get/http://localhost:8001/foundercoefs?dataset=${g.dataSetID}&id=${id}&chrom=${chromosome}&intcovar=${covar}`;
//        effectURL += '&blup=' + $('#performBLUP').is(':checked');
//
//        let lodURL = `http://10.105.9.102/api/get/http://localhost:8001/lodscansamples?dataset=${g.dataSetID}&id=${id}&chrom=${chromosome}&intcovar=${covar}`;

        let effectURL = _API_R_BASE + 'foundercoefs?dataset=' + g.dataSetID + '&id=' + id + '&chrom=' + chromosome + '&intcovar=' + covar;
        effectURL += '&blup=' + $('#performBLUP').is(':checked');

        let lodURL = _API_R_BASE + 'lodscansamples?dataset=' + g.dataSetID + '&id=' + id + '&chrom=' + chromosome + '&intcovar=' + covar;

        effectURL += '&cores=5';
        lodURL += '&cores=5';


        let submitData = {
            urls:[{
                url_id: 'effect',
                url: effectURL,
            }]};

        if (covar !== 'additive') {
            submitData.urls.push({
                url_id: 'lodSamples',
                url: lodURL
            });
        }

        startTask();

        // reset the chart and clear it
        g.chartEffect = {};
        $('#allEffectPlots').html('');

        var q = d3.queue();
        for (var i=0; i<submitData.urls.length; i++) {
            var url = submitData.urls[i];
            q.defer(downloadData2, url.url, url.url_id);
        }

        var group_id = newCallGroup(submitData.urls);
        q.awaitAll(function(error, responseList ) {
            updateGroupResponse(group_id, error, responseList);
        	updateEffectData(group_id, id, chromosome, covar);
        	g.runningTask = true;
            logDebug('group_id=', group_id);
        });

    }

    /**
     * Update the status of the downloading effect data.
     *
     * @param {string} groupID - the group identifier of the task
     */
    function updateEffectData(groupID, id, chromosome, covar) {
        // send GET request to status URL
        logDebug('updateEffectData() ', groupID);

        if (g.runningTask) {
            let statusURL = 'http://ctlin0098.jax.org/';
            $.ajax({
                type: 'GET',
                url: statusURL,
                retries: 3,
                retryInterval: 1000,
                success: function (dataIn, status, request) {
                    var data = getCallGroupResponse(groupID)
                    logDebug('DATA=======', data);
                    if (data.status === 'DONE') {
                        if ('error' in data) {
                            // MAJOR ERROR
                            let message = `Unfortunately, there was a problem contacting the server.  Please try again.`;
                            stopTask();
                            showErrorMessage(message, null);
                        } else if (data.number_tasks_errors !== 0) {
                            let message = `Unfortunately, we encountered an error.  Please try again.`;
                            let errorMessages = '';
                            $.each(data.response_data, function (key, value) {
                                if ('error' in value) {
                                    errorMessages += (`<strong>${key}:</strong> ${value.error}<br>`);
                                }
                            });

                            stopTask();
                            showErrorMessage(message, errorMessages);
                        } else if (data.number_tasks_errors === 0) {
                            // check to make sure the status codes are good
                            let errorMessages = '';
                            $.each(data.response_data, function (key, value) {
                                if (value.status_code !== 200) {
                                    errorMessages += (`<strong>${key}:</strong> ${value.response.error}<br>`);
                                }
                            });

                            if (errorMessages !== '') {
                                let message = `Unfortunately, there was a problem calculating the Effect Plot.`;
                                stopTask();
                                showErrorMessage(message, errorMessages);
                            } else {
                                // show result, there will be 1 datasets to get
                                if (covar === 'additive') {
                                    plotEffectData(data.response_data.effect.response.result, null, chromosome, covar);
                                } else {
                                    plotEffectData(data.response_data.effect.response.result,
                                                   data.response_data.lodSamples.response.result,
                                                   chromosome, covar);
                                }

                                stopTask();
                            }
                        }
                    } else {
                        // rerun in 1 seconds
                        logDebug('Not done, keep checking...');
                        setTimeout(function () {
                            updateEffectData(groupID, id, chromosome, covar);
                        }, 1000);
                    }
                }
            });
        } else {
            // TODO: cleanup
            logDebug('canceling');
            let cancelURL = `/api/cancel/${groupID}`;
            $.getJSON(cancelURL, function (data) {
                logDebug(data);
                g.chartEffect = null;
                $('#allEffectPlots').html('');
            });
        }
    }


    function updateCorrelateDataSet(groupID) {
        // send GET request to status URL
        logDebug('updateCorrelateDataSet');
        logDebug(groupID);

        if (g.runningTask) {
            let statusURL = 'http://ctlin0098.jax.org/';
            $.ajax({
                type: 'GET',
                url: statusURL,
                retries: 3,
                retryInterval: 1000,
                success: function (dataIn, status, request) {
                    var data = getCallGroupResponse(groupID)
                    logDebug('DATA=======', data);
                    if (data.status === 'DONE') {
                        if ('error' in data) {
                            // MAJOR ERROR
                            let message = `Unfortunately, there was a problem contacting the server.  Please try again.`;
                            stopTask();
                            showErrorMessage(message, null);
                        } else if (data.number_tasks_errors !== 0) {
                            let message = `Unfortunately, we encountered an error.  Please try again.`;
                            let errorMessages = '';
                            $.each(data.response_data, function (key, value) {
                                if ('error' in value) {
                                    errorMessages += (`<strong>${key}:</strong> ${value.error}<br>`);
                                }
                            });

                            stopTask();
                            showErrorMessage(message, errorMessages);
                        } else if (data.number_tasks_errors === 0) {
                            // check to make sure the status codes are good
                            let errorMessages = '';
                            $.each(data.response_data, function (key, value) {
                                if (value.status_code !== 200) {
                                    errorMessages += (`<strong>${key}:</strong> ${value.response.error}<br>`);
                                }
                            });

                            if (errorMessages !== '') {
                                let message = `Unfortunately, there was a problem calculating the Correlation Data.`;
                                stopTask();
                                showErrorMessage(message, errorMessages);
                            } else {
                                displayCorrelation(data.response_data.correlation.response.result);
                                stopTask();
                            }
                        }
                    } else {
                        // rerun in 1 seconds
                        logDebug('Not done, keep checking...');
                        setTimeout(function () {
                            updateCorrelateDataSet(groupID);
                        }, 1000);  // TODO: change to 1000 (1 second)
                    }
                }
            });
        } else {
            // TODO: cleanup
            logDebug('canceling');
            let cancelURL = `/api/cancel/${groupID}`;
            $.getJSON(cancelURL, function (data) {
                logDebug(data);
                g.chartCorrelation = null;
                $('#correlationDataTable').html('');
                $('#plotCorrelation').html('');
            });
        }

    }

    function switchCorrelationDataSet(dataSetID, interactiveCovariate) {
        $('#plotCorrelation').html('');
        $('#correlationDataTable').html('');
        g.chartCorrelation = null;
        g.correlationDatasetID = dataSetID;

        let currentID = '';

        if (g.DATASETS[g.dataSetID].datatype === 'mrna') {
            currentID = g.geneID;
        } else if (g.DATASETS[g.dataSetID].datatype === 'protein') {
            currentID = g.proteinID;
        } else if (g.DATASETS[g.dataSetID].datatype === 'pheno') {
            currentID = g.phenotypeID;
        } else {
            // TODO: handle error
            logError('MAJOR PROBLEM');
        }

//        let urlCorrelation = `http://10.105.9.102/api/get/http://localhost:8001/correlation?dataset=${g.dataSetID}&id=${currentID}&dataset_correlate=${dataSetID}`;
        let urlCorrelation = _API_R_BASE + 'correlation?dataset=' + g.dataSetID
            + '&id=' + currentID + '&dataset_correlate=' + dataSetID;

        if ((interactiveCovariate !== undefined) &&
            (interactiveCovariate !== null) &&
            (interactiveCovariate !== 'none')) {
            urlCorrelation += `&intcovar=${interactiveCovariate}`;
        }

        let submitData = {
            urls:[{
                url_id: 'correlation',
                url: urlCorrelation
            }]};

        startTask();


        var q = d3.queue();
        for (var i=0; i<submitData.urls.length; i++) {
            var url = submitData.urls[i];
            q.defer(downloadData2, url.url, url.url_id);
        }

        var group_id = newCallGroup(submitData.urls);
        q.awaitAll(function(error, responseList) {
            updateGroupResponse(group_id, error, responseList);
        	updateCorrelateDataSet(group_id);
        	g.runningTask = true;
            logDebug('group_id=', group_id);
        });
/*
        $.ajax({
            type: 'POST',
            url: '/api/submit',
            contentType: 'application/json',
            data: JSON.stringify(submitData),
            retries: 3,
            retryInterval: 1000,
            success: function(data, status, request) {
                g.runningTask = true;
                logDebug('data=', data);
                logDebug('status=', status);
                logDebug('request=', request);
                logDebug('data.group_id=', data.group_id);
                updateCorrelateDataSet(data.group_id);
            },
            error: function(jqXHR, textStatus, errorThrown) {
                showErrorMessage(errorThrown, textStatus);
            }
        });
*/
    }

    /**
     * Switch the datasets.
     *
     * @param {string} dataSetID - the dataSet identifier
     */
    function switchDataSet(dataSetID) {
        g.dataSetID = dataSetID;

        let html = `<div class="col">
                        <div class="jumbotron jumbotron-fluid" style="background: none">
                            <div class="container">
                                <h1 class="display-3">QTL Viewer</h1>
                                <p class="lead">Please search for a term of interest.</p>
                            </div>
                        </div>
                    </div>`;

        $('#divWelcomeMesage').html(html);

        $('#divItemInfo').addClass('invisible').removeClass('visible');
        $('#divLOD').addClass('invisible').removeClass('visible');
        $('#divSecondRow').addClass('invisible').removeClass('visible');
        $('#divProfileCorrelation').addClass('invisible').removeClass('visible');

        $('#div_current_item_information_header').html('');
        $('#div_current_item_information_body').html('');
        $('#divProfilePlotOptions').html('');

        configureCovarInfo(true);

        setDataSet(dataSetID, true);
    }

    /**
     * Display the gene information.
     * @param {Object} geneData - gene information
     */
    function setDataSet(dataSetID, force) {
        logDebug('setDataSet=', dataSetID);

        if (!(force) && (QTL.g.dataSetID === dataSetID)) {
            logDebug('Same dataset selected, do nothing');
            return;
        }

        plotLODPeaks();

        let ds = g.DATASETS[dataSetID];
        g.dataSetID = dataSetID;

        // clear LOD Plot
        // TODO: clear LOD PLOT, Profile PLOT
        g.chartLOD = null;
        g.chartLODCovariateFull = null;
        g.chartLODCovariateDiff = null;
        g.chartCorrelation = null;
        g.chartEffect = null;
        $('#lodPlotChart').html('');
        $('#lodPlotChartCovariateFull').html('');
        $('#lodPlotChartCovariateDiff').html('');

        resetSecondaryPlots();

        // make tabs invisible
        $('#navGene').hide();
        $('#navPheno').hide();

        // show the correct tab
        if ((ds.datatype === 'mrna') || (ds.datatype === 'protein')) {
            $('#navGene').show();
            $('#navGene').tab('show');

            // show some secondary plots
            $('#navPlotMediation').show();

            // reset the screen
            $('#searchResultsDiv').html('');
            $('#searchResultsTableInfo').html('');

        } else if (ds.datatype === 'pheno') {
            $('#navPheno').show();
            $('#navPheno').tab('show');

            // hide some secondary plots

            // phenotype data can only mediate against mrna or protein (not itself)
            let showMed = false;
            $.each(QTL.g.DATASETS, function(k, v) {
                if ((v.datatype === 'mrna') || (v.datatype === 'protein')) {
                    showMed = true;
                }
            });

            if (showMed) {
                $('#navPlotMediation').show();
            } else {
                $('#navPlotMediation').hide();
            }

            $('#searchPheno').html('');
            $('#searchPhenoCategoryText').html('');
            $('#searchPhenoCategory').html('');
            $('#searchPhenoResultsInfo').html('');
            $('#searchPhenoResultsDiv').html('');

            //let numResults = ds.phenotypes.length;

            // transform the phenotypes into a different structure
            let categories = new Set([]);
            let newPhenotypes = [];
            $.each(ds.phenotypes, function(index, element) {
                if ((element['is.pheno']) && (element['is.numeric'])) {
                    categories.add(element.category);
                    element.display = element['short.name'];
                    newPhenotypes.push({phenotype_id: element['data.name'], // use for id
                                        phenotype: element['short.name'],   // display on screen
                                        category: element.category,
                                        desc: element.description});
                }
            });

            // build the search area
            if (categories.size === 0) {
                logError('No categories - ERROR');
            } else {
                $('#searchPheno').html('<input type="text" id="searchTermPheno" name="searchTermPheno" class="form-control" placeholder="Please enter a phenotype to filter results...">');

                // build the phenotypes table
                $('#searchPhenoResultsDiv').html(`<table id="phenotypesTable" class="table table-striped table-hover table-sm table-bordered">
                                            <thead>
                                                <th style="background-color: #20a8d8" data-dynatable-sorts="Phenotype">Phenotype</th>
                                                <th style="display: none">category</th>
                                                <th style="display: none">desc</th>
                                            </thead>
                                            <tbody>
                                            </tbody>
                                        </table>`);

                g.phenoDynaTable = $('#phenotypesTable')
                    .bind('dynatable:init', function(e, dynatable) {
                        dynatable.queries.functions.phenotype = function(record, queryValue) {
                            if ((record.phenotype.toLowerCase().indexOf(queryValue.toLowerCase()) !== -1) ||
                                (record.desc.toLowerCase().indexOf(queryValue.toLowerCase()) !== -1)) {
                                return true;
                            }
                        };
                    }).dynatable({
                        dataset: {
                            records: newPhenotypes,
                        },
                        features: {
                            paginate: false,
                            pushState: false,
                            recordCount: true,
                            sort: true,
                            search: false,
                        },
                        inputs: {
                            recordCountPlacement: 'before',
                        },
                        writers: {
                            _rowWriter: function(rowIndex, record, columns, cellWriter) {
                                return `<tr><td><span class="font-weight-bold"><a href="#" phenotypeid="${record.phenotype_id}">${record.phenotype}</a></span><br><span class="font-italic">${record.category}</span><br><span class="font-weight-light">${record.desc}</span></td></tr>`;
                            }
                        },
                    }).data('dynatable');

                // build the category select, if more than 1 category
                categories = Array.from(categories);

                if (categories.length > 1) {
                    $('#searchPhenoCategoryText').html('Category Filter');

                    let htmlOptions = '<select id="phenoCategorySelect" data-style="btn-secondary btn-sm" data-width="100%" class="selectpicker">';
                    htmlOptions += '<option>All</option>';
                    $.each(categories, function (i, val) {
                        htmlOptions += `<option>${val}</option>`;
                    });
                    htmlOptions += '</select>';
                    $('#searchPhenoCategory').html(htmlOptions);

                    $('#phenoCategorySelect').selectpicker();

                    $('#phenoCategorySelect').on('changed.bs.select', function (e, clickedIndex, isSelected, previousValue) {
                        let selected = $(this).find(':selected').val();
                        if (selected === 'All') {
                            g.phenoDynaTable.queries.remove('category');
                        } else {
                            g.phenoDynaTable.queries.add('category', selected);
                        }

                        g.phenoDynaTable.process();
                        $('#phenotypesTable tbody a').on('click', function (evt) {
                            evt.preventDefault();
                            let that = $(this);
                            selectPhenotype(that.attr('phenotypeid'), g.dataSetID, $('#interactiveCovarLODS').val());
                            return false;
                        });
                    });
                }

                $('#searchTermPheno').keyup(function (event) {
                    if ( event.which === 13 ) {
                        event.preventDefault();
                    }
                    let q = $(this).val();

                    if (q === '') {
                        q = undefined;
                    }

                    if (q) {
                        g.phenoDynaTable.queries.add('phenotype', q);
                    } else {
                        logDebug('removing');
                        g.phenoDynaTable.queries.remove('phenotype');
                    }

                    logDebug('g.phenoDynaTable.process()');
                    g.phenoDynaTable.process();
                    $('#phenotypesTable tbody a').on('click', function (evt) {
                        evt.preventDefault();
                        let that = $(this);
                        selectPhenotype(that.attr('phenotypeid'), g.dataSetID, $('#interactiveCovarLODS').val());
                        return false;
                    });

                });

                $('#phenotypesTable tbody a').on('click', function (evt) {
                    evt.preventDefault();
                    let that = $(this);
                    selectPhenotype(that.attr('phenotypeid'), g.dataSetID, $('#interactiveCovarLODS').val());
                    return false;
                });
            }

        } else {
            // TODO: handle error
            logDebug('ERROR in setDataSet:', ds);
        }


        // hide or show the mediation based upon if there are datasets that can mediate against
        let allDS = [];
        $.each(g.DATASETS, function(key, value) {
            if (value.datatype !== 'pheno') {
                allDS.push(value);
            }
        });

        $('#divMediationSelect').html('');

        if (allDS.length === 1) {
            $('#divMediationSelect').html(`<p class="font-weight-bold">Mediating against: </p>${allDS[0]['display.name']}`);
            $('#divMediationSelect').append(`<input type="hidden" id="dsMediateAgainstID" value="${allDS[0].id}"/>`);
        } else if (allDS.length > 1) {

            let htmlMediate = `<div class="row">
                                   <div class="col-auto align-self-center font-weight-bold">
                                       Mediate Against
                                  </div>
                                  <div class="col">
                                      <select id="dsMediateAgainstID" data-style="btn-secondary btn-sm" data-width="100%" class="selectpicker">`;

            $.each(allDS, function(idx, elem) {
                logDebug(idx, elem);
                htmlMediate += `<option value="${elem.id}">${elem['display.name']}</option>`;
            });
            htmlMediate += '</select></div></div>';

            $('#divMediationSelect').html(htmlMediate);
            $('#dsMediateAgainstID').selectpicker();
        }
    }

    function exportProfileData(id, data, factors) {
        // TODO: look into changing 'column.name', 'display.name'
        let csvContent = '"mouse.id","expression"';
        $.each(factors, function(k, v) {
            csvContent += `,"${v['display.name']}"`;
        });
        csvContent += '\n';

        $.each(data.data, function(k, v) {
            let line = `"${v['mouse.id']}",${v.expression}`;
            $.each(factors, function(fk, fv) {
                line += `,"${v[fv['sample.column']]}"`;
            });

            csvContent += `${line}\n`;
        });

        downloadCSV(csvContent, `${id}_PROFILE.csv`, 'text/csv;encoding:utf-8');
    }


    /**
cccc
     * Plot profile data.
     * @param {string} id - the unique identifier
     */
    function plotProfile(id) {
    	logInfo('plotProfile: ' + id);

        let data = g.expressionData;

        // categories will be the order the user selected
        let selectedCovars = getFactorOrder();

        // categorySeries will be how to color
        let seriesToColor = $('#factorSeries').find(':selected').val();

        logDebug('selectedCovars=', selectedCovars);
        logDebug('seriesToColor=', seriesToColor);

        if ((selectedCovars === null) || (selectedCovars.length === 0)) {
            // this is the case when there are no covar factors

            // do we just have 1 series of data here?
        } else {
            // just mapping column name to display name
            let cfMap = {};

            $.each(g.DATASETS[g.dataSetID]['covar.info'], function (idx, elem) {
                cfMap[elem['sample.column']] = elem['display.name'];
            });

            let title = [];

            let covarPermutations = [];

            $.each(selectedCovars, function (idx, elem) {
            	logInfo(idx + ' ' + elem)
                covarPermutations.push(data.datatypes[elem]);
                title.push(cfMap[elem]);
            });

            let chartTitle = title.join(' x ');
            covarPermutations = permutateArrays(covarPermutations);

            logDebug('covarPermutations=', covarPermutations);

            let covarCategories = [];
            let covarCategoriesMap = {};
            let categoryData = {};  // for box plot

            $.each(covarPermutations, function (idx, elem) {
                let catName = elem.join(':');
                covarCategories.push(catName);
                covarCategoriesMap[catName] = idx;
                categoryData[catName] = [];
            });

            logDebug('covarCategories=', covarCategories);
            logDebug('covarCategoriesMap=', covarCategoriesMap);

            let seriesData = {};

            $.each(data.datatypes[seriesToColor], function (idx, val) {
                seriesData[val] = {values: []};
            });



            $.each(data.data, function (idx, val) {
                if (val.expression !== undefined) {
                    let t = [];

                    $.each(selectedCovars, function (i, v) {
                        t.push(val[v]);
                    });

                    let catName = t.join(':');

                    seriesData[val[seriesToColor]].values.push({
                        x: covarCategoriesMap[catName],
                        y: val.expression,
                        obj: val
                    });

                    categoryData[catName].push(val.expression);
                }
            });

            logDebug('seriesData=', seriesData);

            // convert from object, to array

            let series = [];
            let colors = ['#7cb5ec', '#ed3215', '#8085e9', '#f7a35c', '#90ed7d',
                '#e4d354', '#2b908f', '#f45b5b', '#91e8e1'];

            let boxSeries = [];

            $.each(seriesData, function (key, val) {
                series.push({
                    data: val.values,
                    name: key,
                    marker: {
                        radius: 2
                    },
                    color: colors[series.length % colors.length],
                    jitter: {
                        x: 0.24
                    },
                    zIndex: 10
                });
            });

            $.each(categoryData, function (key, val) {
                if (val.length > 0) {
                    let boxValues = getBoxValues(val);

                    boxValues.x = covarCategoriesMap[key];
                    boxValues.name = key;
                    boxValues.enableMouseTracking = false;
                    boxSeries.push(boxValues);
                }
            });

            series.push({
                type: 'boxplot',
                data: boxSeries,
                showInLegend: false,
                zIndex: 1,
            });

            let exporting = appendExportButton('Download CSV', function () {
                exportProfileData(id, data, g.DATASETS[g.dataSetID]['covar.info']);
            });
            exporting.filename = id + '_PROFILE';

            Highcharts.chart({
                chart: {
                    type: 'scatter',
                    renderTo: 'profilePlotChart',
                },

                title: {
                    text: chartTitle,
                    verticalAlign: 'bottom'
                },

                xAxis: {
                    // min and max forces the "categories" to be shown even when no data is available
                    //min: 0,
                    //max: point[point.length-1] + jitter + jitterPad,
                    //tickPositions: point,
                    categories: covarCategories,
                    labels: {
                        rotation: -45
                    }
                },

                yAxis: {
                    title: {
                        text: id
                    }
                },

                series: series,

                exporting: exporting,

                plotOptions: {
                    boxplot: {
                        color: '#000000',
                        enableMouseTracking: false,
                        lineWidth: 1,
                        medianColor: '#000000',
                        medianWidth: 1,
                        stemColor: '#000000',
                        stemWidth: 1,
                        whiskerColor: '#000000',
                        whiskerLength: '50%',
                        whiskerWidth: 1
                    },
                    scatter: {
                        tooltip: {
                            headerFormat: null,
                            outside: true,
                            pointFormatter: function () {
                                let val = this.obj;
                                let hoverText = `<b>${val['mouse.id']}</><br><p>${val.expression}</p>`;

                                $.each(data.datatypes, function (i, e) {
                                    hoverText += `<br><p>${cfMap[i]}: ${val[i]}</p>`;
                                });

                                return hoverText;
                            }


                        }
                    }
                }
            });
        }

    }

    /**
     * Perform gene search.
     */
    function findGene(searchVal) {
        //let button = $('#btnGo');
        let term = $('#searchTerm');
        term.disable(true);
        //button.button('loading');

        if (searchVal.length === 0) {
            term.focus();
            term.disable(false);
            //button.button('reset');
            return;
        }

        let options = {
            species: g.SPECIES_ID,
            release: g.ENSEMBL_RELEASE,
            limit: 100
        };

        logDebug(options);

        startTask();
        g.ENSIMPL.search(searchVal, options, searchCallback);
    }

    /**
     * Populate the search results.
     */
    function searchCallback() {
        logDebug(g.ENSIMPL);

        if (g.ENSIMPL.response.result.matches === null) {
            $('#searchResultsDiv').html('');
            $('#btnGo').button('reset');
            $('#searchTerm').disable(false);
            $('#searchResultsTableInfo').html('No results found');
            stopTask();
            return;
        }

        let tbl = '<table id="searchResultsTable" class="table table-striped table-hover table-sm table-bordered">';
        tbl += '<thead>';
        tbl += '<tr>';
        tbl += '<th class="" scope="col">ID</th>';
        tbl += '<th class="" scope="col">Symbol</th>';
        /*
        tbl += '<th class="d-none d-md-table-cell" scope="col">Position</th>';
        tbl += '<th class="d-none d-xl-table-cell" scope="col">Match Reason</th>';
        tbl += '<th class="d-none d-xl-table-cell" scope="col">Description</th>';
        */
        tbl += '</tr>';
        tbl += '</thead>';
        tbl += '<tbody id="tblBody">';
        tbl += '</tbody>';
        tbl += '</table>';
        $('#searchResultsDiv').html(tbl);

        let currentDataSet = g.DATASETS[g.dataSetID];
        let response = g.ENSIMPL.response;
        let searchResults = {};

        $.each(response.result.matches, function(idx, match) {
            let row = '<tr>';
            searchResults[match.ensembl_gene_id] = match;

            if (currentDataSet.datatype === 'mrna') {
                if (currentDataSet.gene_ids[match.ensembl_gene_id]) {
                    row += `<td class="text-nowrap"><i id="matchInfo" geneID="${match.ensembl_gene_id}" class="fas fa-info-circle"></i> <a href="#" geneID="${match.ensembl_gene_id}">${match.ensembl_gene_id}</a></td>`;
                } else {
                    row += `<td class="text-nowrap"><i id="matchInfo" geneID="${match.ensembl_gene_id}" class="fas fa-info-circle"></i> ${match.ensembl_gene_id}</td>`;
                }
            } else if (currentDataSet.datatype === 'protein') {
                if (currentDataSet.gene_ids[match.ensembl_gene_id]) {
                    let ps = [];
                    row += `<td><div class="text-nowrap"><i id="matchInfo" geneID="${match.ensembl_gene_id}" class="fas fa-info-circle"></i> ${match.ensembl_gene_id}</data>`;
                    row += '<div style="padding-left: 20px;">';

                    for (let i in currentDataSet.gene_ids[match.ensembl_gene_id].protein_ids) {
                        let p = currentDataSet.gene_ids[match.ensembl_gene_id].protein_ids[i];
                        ps.push(`<em><a href="#" geneID="${match.ensembl_gene_id}" proteinID="${p}">${p}</a></em>`);
                    }

                    row += ps.join('<br>');
                    row += '</div></td>';
                } else {
                    row += `<td class="text-nowrap"><i id="matchInfo" geneID="${match.ensembl_gene_id}" class="fas fa-info-circle"></i> ${match.ensembl_gene_id}</td>`;
                }
            } else {
                // TODO: handle error
                logError('THIS SHOULD NOT HAPPEN');
            }

            row += `<td>${match.symbol}</td>`;
            /*
            row += '<td class="d-none d-md-table-cell">' + match.symbol + '</td>';
            row += '<td class="d-none d-xl-table-cell">' + match.symbol + '</td>';
            row += '<td class="d-none d-xl-table-cell">' + match.symbol + '</td>';
            */
            row += '</tr>';

            $('#tblBody').append(row);
        });

        if (response.result.num_results === 1) {
            $('#searchResultsTableInfo').html(response.result.matches.length.toLocaleString() + ' result');
        } else if (response.result.matches.num_matches < 100) {
            $('#searchResultsTableInfo').html(response.result.matches.length.toLocaleString() + ' results');
        } else {
            $('#searchResultsTableInfo').html('Showing first ' + response.result.matches.length.toLocaleString() + ' of ' + response.result.num_results.toLocaleString() + ' results');
        }

        $('#searchResultsTable a').on('click', function (evt) {
            let that = $(this);
            evt.preventDefault();
            selectGeneProtein(that.attr('geneid'), that.attr('proteinid'), g.dataSetID, $('#interactiveCovarLODS').val());
            return false;
        });

        setTimeout(function () {
            $('[id="matchInfo"]').each(function (idx, elem) {
                $(this).popover({
                    placement: 'right',
                    html: true,
                    trigger: 'hover',
                    container: 'body',
                    sanitize: false,
                    content: function () {
                        let d = searchResults[elem.getAttribute('geneid')];
                        let h = `<table class="table table-sm">
                                    <tr><td><strong>Symbol</strong> </td><td>${d.symbol}</td></tr>
                                    <tr><td><strong>Location</strong> </td><td>${d.chromosome}:${d.position_start}-${d.position_end}</td></tr>
                                    <tr><td><strong>Name</strong> </td><td>${d.name}</td></tr>
                                    <tr><td><strong>Synonyms</strong> </td><td>`;

                        let s = d.synonyms;
                        if (s) {
                            if (s.length > 5) {
                                let s2 = s.slice(1, 6);
                                h += s2.join('<br>');
                                h += '<br><i>(' + (s.length - s2.length) + ' more synonyms)</i>';
                            } else {
                                h += s.join('<br>');
                            }
                        }
                        h += `</td></tr>
                              <tr><td><strong>Match Reason</strong> </td><td>${d.match_reason}</td></tr>
                              <tr><td><strong>Match Value</strong> </td><td>${d.match_value}</td></tr>
                              </table>`;

                        return h;
                    }
                });
            });

        });

        g.searchResults = searchResults;
        $('#btnGo').button('reset');
        $('#searchTerm').disable(false);

        stopTask();

        // simulate a click event if the search yields only 1 result
        if ($('#searchResultsTable a').length === 1) {
            $('#searchResultsTable a')[0].click();
        }

        $('#searchResultsTable').DataTable({
            "info": false,
            "filter": false,
            "scrollY": "300px",
            "false": true,
            "order": [],
            "paging": false,
        });
    }

    function generateSecondaryPlot(plot, id, markerID, chromosome, location, covar) {
        logDebug('Generating Secondary Plot' + plot);
        if (plot === 'navPlotMediation') {
            generateMediationPlot(id, markerID, covar);
        } else if (plot === 'navPlotEffect') {
            generateEffectPlot(id, chromosome, covar);
        } else if (plot === 'navPlotSNPs') {
            generateSNPAssocPlot(id, chromosome, location, covar);
        } else {
            logError('Unknown plot ', plot);
        }
    }

    // The download function takes a CSV string, the filename and mimeType as parameters
    // Scroll/look down at the bottom of this snippet to see how download is called
    function downloadCSV(content, fileName, mimeType) {
        let a = document.createElement('a');
        mimeType = mimeType || 'application/octet-stream';

        if (navigator.msSaveBlob) { // IE10
            navigator.msSaveBlob(new Blob([content], {
                type: mimeType
            }), fileName);
        } else if (URL && 'download' in a) { //html5 A[download]
            a.href = URL.createObjectURL(new Blob([content], {
                type: mimeType
            }));
            a.setAttribute('download', fileName);
            document.body.appendChild(a);
            a.click();
            document.body.removeChild(a);
        } else {
            location.href = 'data:application/octet-stream,' + encodeURIComponent(content); // only this mime type is supported
        }
    }


    function exportLODChartData(id, data, covar, exportName) {
        let csvContent = '"id","chromosome","position","lod"\n';
        $.each(data, function(k, v) {
            csvContent += `"${v.id}","${v.chr}",${v.chrPos},${v.y}\n`;
        });

        if (exportName !== null) {
            downloadCSV(csvContent, `${id}_LOD_${covar}_${exportName}.csv`, 'text/csv;encoding:utf-8');
        } else {
            downloadCSV(csvContent, `${id}_LOD.csv`, 'text/csv;encoding:utf-8');
        }

    }

    function plotLODChart(dataLOD, covar, dataDiff) {
        // "1_3000000", "1", 3, 1.4975
        let maxLOD = -Infinity;
        let minLOD = Infinity;
        let xAxisTitle = '';
        let xAxisSubTitle = '';
        let newLodData = [];
        let currentID = null;
        let isCovar = false;
        let renderTo = 'lodPlotChart';
        let lineColor = 'black';
        let isFull = true;
        let exportName = null;

        if (covar !== 'additive') {
            isCovar = true;
            renderTo = 'lodPlotChartCovariateFull';
            exportName = 'FULL';

            if (dataDiff !== null) {
                lineColor = '#146582';
                isFull = false;
                renderTo = 'lodPlotChartCovariateDiff';
                exportName = 'DIFF';
            }
        } else {
            //
        }

        logDebug('dataLOD=', dataLOD);
        logDebug('covar=', covar);
        logDebug('dataDiff=', dataDiff);
        logDebug('renderTo=', renderTo);

        if (dataLOD !== null) {
            maxLOD = dataLOD[0][3];
            minLOD = -2;

            if (g.DATASETS[g.dataSetID].datatype === 'mrna') {
                currentID = g.geneID;
                xAxisTitle = g.geneID + ' (' + g.gene.gene[g.geneID].symbol + ')';

            } else if (g.DATASETS[g.dataSetID].datatype === 'protein') {
                currentID = g.proteinID;
                xAxisTitle = g.proteinID + ' (' + g.gene.gene[g.geneID].symbol + ')';
            } else if (g.DATASETS[g.dataSetID].datatype === 'pheno') {
                // pheno
                xAxisTitle = g.phenotypeID;
                currentID = g.phenotypeID;
                if (currentID !== g.DATASETS[g.dataSetID].phenotypes[currentID]['short.name']) {
                    xAxisTitle += (' (' + g.DATASETS[g.dataSetID].phenotypes[currentID]['short.name'] + ')');
                }
            } else {
                // TODO: handle error
                logError('MAJOR PROBLEM');
            }

            if (isCovar) {
                xAxisSubTitle = `${covar} ${exportName}`;
            }

            $.each(dataLOD, function(index, element) {
                //
                // element = {"[id]","[chr]",[position],[data]}
                //
                if (element[1] in g.CHROMOSOMES.chr) {

                    let chr = g.CHROMOSOMES.chr[element[1]];
                    let x = Math.round(chr.start + (element[2] * 1000000.0));
                    let y = element[3];

                    if (!isFull) {
                        y = y - dataDiff[index][3];
                    }

                    let d = {
                        x: x,
                        y: y,
                        id: element[0],
                        text: element[3],
                        chr: chr.chromosome,
                        chrPos: element[2],
                        covar: covar
                    };

                    newLodData.push(d);
                    maxLOD = Math.max(element[3], maxLOD);
                }
            });

            logDebug('MAX LOD = ', maxLOD);

            maxLOD = Math.ceil(maxLOD) + 1;

            logDebug('MAX LOD = ', maxLOD);
        }

        newLodData.sort(compareX);

        logDebug('newLodData=', newLodData);

        //
        // build the vertical chromosome bars
        //
        let chromosomeAxisVals = [];
        let chromosomeAxisText = {};
        let chromosomeBands = [];

        $.each(g.CHROMOSOMES.idx, function(index, element) {

            chromosomeAxisVals.push(element.mid);
            chromosomeAxisText[element.mid] = element.chromosome;

            // TODO: style this
            let fillColor = '#eeeeee';
            if ((index % 2) === 1) {
                chromosomeBands.push({
                    color: fillColor,
                    from: element.start,
                    to: element.end
                });
            }
        });

        let plotShapes = [];
        let plotAnnotations = [];




        let exporting = appendExportButton('Download CSV', function() {
            exportLODChartData(currentID, newLodData, covar, exportName);
        });

        exporting.filename = currentID + '_LOD';

        if (isCovar) {
            exporting.filename = `${currentID}_LOD_${covar}_${exportName}`;
        }

        g.LODAdditiveMax = maxLOD;

        let series = {
                boostThreshold: 1,
                color: lineColor,
                data: newLodData,
                name: covar,
                lineWidth: 0.5,
                marker: {
                   enabled: false
                },
                showInLegend: false,
                turboThreshold: 0,
                type: 'line',
            };

        let chartLOD = Highcharts.chart({
            boost: {},
            chart: {
                alignTicks: false,
                renderTo: renderTo,
                zoomType: 'x',
                events: {
                    click: function(event) {
                        if (event.target.tagName === 'tspan') {
                            // do not generate secondary plot for user clicking "Reset zoom"
                            return;
                        }

                        let plot = $('#secondaryPlotTabs').find('.active').attr('id');
                        let markerID = this.hoverPoint.id;
                        let chromosome = this.hoverPoint.chr;
                        let location = this.hoverPoint.chrPos;
                        let covar = this.hoverPoint.covar;

                        g.secondaryPlotMarkerID = markerID;
                        g.secondaryPlotChromosome = chromosome;
                        g.secondaryPlotLocation = location;
                        g.secondaryPlotCovar = covar;

                        logDebug('event', markerID, chromosome, location, this.hoverPoint.covar);

                        generateSecondaryPlot(plot, currentID, markerID, chromosome, location, covar);
                    }
                }
            },
            exporting: exporting,
            plotOptions: {
                series: {
                    findNearestPointBy: 'xy',
                    cursor: 'pointer',
                    events: {
                        click: function(event) {
                            logDebug('plotOptions, series', event);

                            let plot = $('#secondaryPlotTabs').find('.active').attr('id');
                            let markerID = event.point.id;
                            let chromosome = event.point.chr;
                            let location = event.point.chrPos;
                            let covar = event.point.covar;

                            g.secondaryPlotMarkerID = markerID;
                            g.secondaryPlotChromosome = chromosome;
                            g.secondaryPlotLocation = location;
                            g.secondaryPlotCovar = covar;

                            logDebug('series', markerID, chromosome, location, event.point.covar);

                            generateSecondaryPlot(plot, currentID, markerID, chromosome, location, covar);

                        }
                    }
                }
            },

            series: [series],
            title: {
                text: xAxisTitle
            },
            subtitle: {
                text: xAxisSubTitle
            },
            tooltip: {
                outside: true,
                /*
                positioner: function(labelWidth, labelHeight, point) {
                    let tooltipX, tooltipY;
                    let chart = chartLOD;

                    if (point.plotX + labelWidth > chart.plotWidth) {
                        tooltipX = point.plotX + chart.plotLeft - labelWidth - 20;
                    } else {
                        tooltipX = point.plotX + chart.plotLeft + 20;
                    }

                    if (point.plotY + labelHeight > chart.plotHeight) {
                        tooltipY = point.plotY + chart.plotTop - labelHeight - 20;
                    } else {
                        tooltipY = point.plotY + chart.plotTop - 20;
                    }

                    return {
                        x: tooltipX,
                        y: tooltipY
                    };
                },
                */
                useHTML: true,
                formatter: function () {
                    let p = this.point;
                    return `<b>${p.id}</b><br/>LOD: ${p.y}<br/>Chromosome: ${p.chr}<br/>Position: ${p.chrPos}`;
                }
            },
            xAxis: {
                opposite: true,
                gridLineWidth: 0,
                labels: {
                    formatter: function () {
                        return chromosomeAxisText[this.value];
                    }
                },
                tickPositions: chromosomeAxisVals,
                plotBands: chromosomeBands,

            },
            yAxis: {
                min: 0,
                max: maxLOD,
                title: {
                    text: 'LOD'
                },

            },
        });

        if (isCovar) {
            if (isFull) {
                g.chartLODCovariateFull = chartLOD;
            } else {
                g.chartLODCovariateDiff = chartLOD;
            }
        } else {
            g.chartLOD = chartLOD;
        }
    }



    /**
     * Get the selected factors.
     * @returns {Array} an array of strings
     */
    function getFactorOrder() {
        let selected = [];
        $('#factor-view-select option:selected').each(function() {
            selected.push([$(this).val(), $(this).attr('order')]);
        });

        selected.sort(function(a, b) {
            return a[1] - b[1];
        });

        let ordered = [];
        for (let i = 0; i < selected.length; i++) {
            ordered.push(selected[i][0]);
        }

        return ordered;
    }


    function configureCovarInfo(resetPeaks) {
        // current dataset
        logDebug('configureCovarInfo', resetPeaks);
        let ds = g.DATASETS[g.dataSetID];

        if (ds['covar.info'] === null) {
            logDebug('NO COVAR INFO');

            let profilePlotOptionsHTML = `
            <input type="hidden" id="factorSeries" value="additive"/>
            <input type="hidden" id="factorSeriesCorrelation" value="additive"/>
            '<select id="factorSeries" style="hidden"><option value="" selected></option></select>';
            `;

            $('#divInteractiveCovariatesLOD').html(`<input type="hidden" id="interactiveCovarLODS" value="additive"/>`);

            if (resetPeaks) {
                $('#divInteractiveCovariatesPeaks').html(`<input type="hidden" id="interactiveCovarPeaks" value="additive"/>`);
            }

            $('#divProfilePlotOptions').html(profilePlotOptionsHTML);

            $('#correlationCovariateSelectionText').addClass('hidden').removeClass('visible');
            $('#correlationCovariateSelection').html('<input type="hidden" id="interactiveCovarCorrelation" value="additive"/>');


            return;
        }

        let profilePlotOptionsHTML = `
                                  <div class="col">
                                    <div class="row">
                                        <div class="col font-weight-bold">
                                            Select your factors
                                        </div>
                                    </div>

                                    <div class="row">
                                        <div class="col">
                                            <div id="factor-view-select-wrapper">
                                                <select id="factor-view-select" multiple="multiple">
                                                </select>
                                            </div>
                                        </div>
                                    </div>

                                    <div class="row row-spacer"></div>

                                    <div class="row">
                                        <div class="col font-weight-bold">
                                            Select a series to color
                                        </div>
                                    </div>

                                    <div class="row">
                                        <div class="col" id="divFactorSeries">
                                        </div>
                                    </div>

                                    <div class="row row-spacer"></div>
</div>`;

        $('#divProfilePlotOptions').html(profilePlotOptionsHTML);
        g.orderCount = ds['covar.info'].length;

        logDebug('g.orderCount=',g.orderCount);


        $('#factor-view-select').multiselect({
            onChange: function(option, checked) {
                if (checked) {
                    g.orderCount++;
                    $(option).attr('order', g.orderCount);
                }
                else {
                    $(option).attr('order', '');
                }



                if (g.DATASETS[g.dataSetID].datatype === 'mrna') {
                    plotProfile(g.geneID);
                } else if (g.DATASETS[g.dataSetID].datatype === 'protein') {
                    plotProfile(g.proteinID);
                } else if (g.DATASETS[g.dataSetID].datatype === 'pheno') {
                    plotProfile(g.phenotypeID);
                } else {
                    // TODO: major error
                    logError('MAJOR ERROR');
                }

            },
            maxHeight: 400,
            dropUp: false,
            buttonWidth: '100%',
            buttonClass: 'text-left btn btn-sm btn-secondary',
            buttonText: function(options) {
                if (options.length === 0) {
                    return 'None selected';
                }
                else {
                    let selected = [];

                    options.each(function() {
                        selected.push([$(this).text(), $(this).attr('order')]);
                    });

                    selected.sort(function(a, b) {
                        return a[1] - b[1];
                    });

                    let text = '';
                    for (let i = 0; i < selected.length; i++) {
                        // text += ((i+1) + ': ' + selected[i][0] + ', ');  // with numbers
                        text += (selected[i][0] + ', ');  // without numbers
                    }

                    return text.substr(0, text.length - 2);
                }
            }
        });


        let selectOptions = [];
        let html = '<select id="factorSeries" data-style="btn-secondary btn-sm" data-width="100%" class="selectpicker">';
        let htmlCorrelation = '<select id="factorSeriesCorrelation" data-style="btn-secondary btn-sm" data-width="100%" class="selectpicker">';
        let htmlInteractiveCovariatesLOD = `<div class="row">
                                                <div class="col-auto align-self-center font-weight-bold">
                                                    <span>Plots</span>
                                                </div>
                                                <div class="col">
                                                    <select id="interactiveCovarLODS" data-style="btn-secondary btn-sm" data-width="100%" class="selectpicker">
                                            `;
        let htmlInteractiveCovariatesPeaks = `<div class="row">
                                                  <div class="col-auto align-self-center">
                                                    <span>Plot type</span>
                                                  </div>
                                                  <div class="col">
                                                      <select id="interactiveCovarPeaks" data-style="btn-secondary btn-sm" data-width="100%" class="selectpicker">
                                        `;

        let htmlInteractiveCovariatesCorrelation = `<select id="interactiveCovarCorrelation" data-style="btn-secondary btn-sm" data-width="100%" class="selectpicker">`;

        g.interactiveCovariates = [];

        htmlInteractiveCovariatesLOD += `<option value="additive">Additive</option>`;
        htmlInteractiveCovariatesPeaks += `<option value="additive">Additive</option>`;
        htmlInteractiveCovariatesCorrelation += `<option value="none">None</option>`;

        $.each(ds['covar.info'], function(index, element) {
            selectOptions.push({
                label: element['display.name'],
                order: index,
                selected: element.primary,
                title: element['display.name'],
                value: element['sample.column'],
            });

            html += `<option value="${element['sample.column']}">${element['display.name']}</option>`;
            htmlCorrelation += `<option value="${element['sample.column']}">${element['display.name']}</option>`;

            // interactive covariates
            if (element.interactive) {
                g.interactiveCovariates.push(element);
                htmlInteractiveCovariatesLOD += `<option value="${element['sample.column']}">${element['display.name']}</option>`;
                htmlInteractiveCovariatesPeaks += `<option value="${element['sample.column']}">${element['display.name']}</option>`;
                htmlInteractiveCovariatesCorrelation += `<option value="${element['sample.column']}">${element['display.name']}</option>`;
            }
        });

        html += '</select>';
        htmlCorrelation += '</select>';
        htmlInteractiveCovariatesLOD += '</select></div></div>';
        htmlInteractiveCovariatesPeaks += '</select></div></div>';
        htmlInteractiveCovariatesCorrelation += '</select>';

        $('#factor-view-select').multiselect('dataprovider', selectOptions);

        $('#divFactorSeries').html(html);
        $('#divFactorSeriesCorrelation').html(htmlCorrelation);


        logDebug('g.interactiveCovariates=', g.interactiveCovariates);

        // display a selection if we have interactive covariates, nothing if not
        if (g.interactiveCovariates.length > 0) {
            $('#divInteractiveCovariatesLOD').html(htmlInteractiveCovariatesLOD);

            $('#interactiveCovarLODS').selectpicker();

            $('#interactiveCovarLODS').on('changed.bs.select', function (e, clickedIndex, isSelected, previousValue) {
                changeCovariate(e.target.value);
            });

            // add Covariate adjustment for correlation
            $('#correlationCovariateSelectionText').addClass('visible').removeClass('hidden');
            $('#correlationCovariateSelection').html(htmlInteractiveCovariatesCorrelation);
            $('#interactiveCovarCorrelation').selectpicker();

            $('#interactiveCovarCorrelation').on('changed.bs.select', function (e, clickedIndex, isSelected, previousValue) {
                let selected = $(this).find(':selected').val();
                let dataset = $('#correlationDatasetSelect').find(':selected').val();
                switchCorrelationDataSet(dataset, selected);
            });


            if (resetPeaks) {
                $('#divInteractiveCovariatesPeaks').html(htmlInteractiveCovariatesPeaks);

                $('#interactiveCovarPeaks').selectpicker();

                $('#interactiveCovarPeaks').on('changed.bs.select', function (e, clickedIndex, isSelected, previousValue) {
                    updatePeaks(+$('#lodSliderValue').val());
                });
            }



        } else {
            $('#divInteractiveCovariatesLOD').html(`<input type="hidden" id="interactiveCovarLODS" value="additive"/>`);

            // remove Covariate adjustment for correlation
            $('#correlationCovariateSelectionText').addClass('hidden').removeClass('visible');
            $('#correlationCovariateSelection').html('<input type="hidden" id="interactiveCovarCorrelation" value="additive"/>');

            if (resetPeaks) {
                $('#divInteractiveCovariatesPeaks').html(`<input type="hidden" id="interactiveCovarPeaks" value="additive"/>`);
            }
        }

        $('#factorSeries').selectpicker();

        $('#factorSeries').on('changed.bs.select', function (e, clickedIndex, isSelected, previousValue) {
            if (g.DATASETS[g.dataSetID].datatype === 'mrna') {
                plotProfile(g.geneID);
            } else if (g.DATASETS[g.dataSetID].datatype === 'protein') {
                plotProfile(g.proteinID);
            } else if (g.DATASETS[g.dataSetID].datatype === 'pheno') {
                plotProfile(g.phenotypeID);
            } else {
                // TODO: major error
                logError('MAJOR ERROR');
            }
        });

        $('#factorSeriesCorrelation').selectpicker();

        $('#factorSeriesCorrelation').on('changed.bs.select', function (e, clickedIndex, isSelected, previousValue) {
            plotCorrelationChart();
        });

        $('#btnDownloadCorrelationData').on('click', function(evt) {
            if (g.correlationData) {
                let csvContent = '';
                let id = '';

                if (g.DATASETS[g.dataSetID].datatype === 'mrna') {
                    id = g.geneID;
                } else if (g.DATASETS[g.dataSetID].datatype === 'protein') {
                    id = g.proteinID;
                } else if (g.DATASETS[g.dataSetID].datatype === 'pheno') {
                    id = g.phenotypeID;
                } else {
                    logError('MAJOR ERROR');
                }

                if (g.DATASETS[g.correlationDatasetID].datatype === 'mrna') {
                    csvContent = '"id","symbol","chr","start","end","correlation"\n';
                    $.each(g.correlationData, function (k, v) {
                        csvContent += `"${v.id}","${v.symbol}","${v.chr}",${v.start},${v.end},${v.cor}\n`;
                    });
                } else if (g.DATASETS[g.correlationDatasetID].datatype === 'protein') {
                    csvContent = '"id","symbol","chr","start","end","correlation"\n';
                    $.each(g.correlationData, function (k, v) {
                        csvContent += `"${v.id}","${v.symbol}","${v.chr}",${v.start},${v.end},${v.cor}\n`;
                    });
                } else if (g.DATASETS[g.correlationDatasetID].datatype === 'pheno') {
                    csvContent = '"id","correlation"\n';
                    $.each(g.correlationData, function (k, v) {
                        csvContent += `"${v.id}",${v.cor}\n`;
                    });
                } else {
                    // TODO: major error
                    logError('MAJOR ERROR');
                }

                downloadCSV(csvContent, `${id}_correlation.csv`, 'text/csv;encoding:utf-8');
            } else {
                logDebug('no correlation data?');
            }
        });

    }

    function formatStrToNum(val, decimals) {
        return Number.parseFloat(val).toFixed(decimals);
    }


    function displayCorrelation(correlationData) {
        $('#correlationDataTable').html('');



        logDebug('displayCorrelation()');
        logDebug('correlationData=', correlationData);
        g.correlationData = correlationData;

        logDebug('g.DATASETS[g.correlationDatasetID].datatype=', g.DATASETS[g.correlationDatasetID].datatype);

        let htmlBody = '<table id="corTable" class="table table-striped table-bordered table-sm table-sm-text">';

        if (g.DATASETS[g.correlationDatasetID].datatype === 'mrna') {
            htmlBody += '<thead><th>Value</th><th>Symbol</th><th>Location</th></thead>';
            $.each(correlationData, function(idx, value) {
                if (idx < 1000) {
                    htmlBody += '<tr><td data-order="' + Math.abs(formatStrToNum(value.cor, 4)) + '">' + formatStrToNum(value.cor, 4) + `</td><td><a href="#" dSymbol="${value.symbol}" dID="${value.id}">${value.symbol}</a></td><td>${value.chr}:` + formatStrToNum(value.start, 2) + '-' + formatStrToNum(value.end, 2) + '</td></tr>';
                }
            });
        } else if (g.DATASETS[g.correlationDatasetID].datatype === 'protein') {
            htmlBody += '<thead><th>Value</th><th>Symbol</th><th>Location</th></thead>';
            $.each(correlationData, function(idx, value) {
                if (idx < 1000) {
                    htmlBody += '<tr><td data-order="' + Math.abs(formatStrToNum(value.cor, 4)) + '">' + formatStrToNum(value.cor, 4) + `</td><td><a href="#" dSymbol="${value.symbol}" dID="${value.id}">${value.symbol}</a></td><td>${value.chr}:` + formatStrToNum(value.start, 2) + '-' + formatStrToNum(value.end, 2) + '</td></tr>';
                }
            });
        } else if (g.DATASETS[g.correlationDatasetID].datatype === 'pheno') {
            htmlBody += '<thead><th>Value</th><th>ID</th></thead>';
            $.each(correlationData, function(idx, value) {
                if (idx < 1000) {
                    htmlBody += '<tr><td data-order="' + Math.abs(formatStrToNum(value.cor, 4)) + '">' + formatStrToNum(value.cor, 4) + `</td><td><a href="#" dSymbol="" dID="${value.id}">${value.id}</a></td></tr>`;
                }
            });
        } else {
            // TODO: major error
            logError('MAJOR ERROR');
        }

        htmlBody += '</table>';
        $('#correlationDataTable').html(htmlBody);

        $('#corTable').DataTable({
            "info":     false,
            "language": {
                "search": "Filter"
            },
            "scrollY":        "200px",
            "scrollCollapse": true,
            "order": [[ 0, "desc" ]],
            "paging":         false,
            "columnDefs": [{
                "targets": 0,
                "className": "dt-body-right"
            }]
        });


        $('#corTable a').click(function(evt) {
            evt.preventDefault();
            let that = $(this);
            let cid = null;

            if (g.DATASETS[g.dataSetID].datatype === 'mrna') {
                cid = g.geneID;
            } else if (g.DATASETS[g.dataSetID].datatype === 'protein') {
                cid = g.proteinID;
            } else if (g.DATASETS[g.dataSetID].datatype === 'pheno') {
                cid = g.phenotypeID;
            }

            let interactiveCovariate = $('#interactiveCovarCorrelation').find(':selected').val();

            if (interactiveCovariate === 'none') {
                interactiveCovariate = null;
            }

            generateCorrelationPlot(g.dataSetID, cid, $('#correlationDatasetSelect').val(), that.attr('dID'), that.attr('dSymbol'), interactiveCovariate);
            return false;
        });



    }

    function exportCorrelationPlotData(id, idCorrelate, data) {
        let csvContent = `"mouse.id","${id}","${idCorrelate}"\n`;

        $.each(data, function(k, v) {
            csvContent += `"${v['mouse.id']}",${v.x},${v.y}\n`;
        });

        downloadCSV(csvContent, `${id}_${idCorrelate}_CORRELATION.csv`, 'text/csv;encoding:utf-8');
    }


    function plotCorrelationChart() {
        let data = g.correlationChartData;
        logDebug('plotCorrelationChart, data=', data);

        let categorySeries = $('#divFactorSeriesCorrelation').find(':selected').val();
        logDebug('categorySeries=', categorySeries);

        let seriesData = {};

        // need to pass  back factors or use global?
        $.each(data.datatypes[categorySeries], function(idx, val) {
            seriesData[val] = { values: [] };
        });

        let minValue = Infinity;
        let maxValue = -Infinity;

        $.each(data.data, function(idx, element) {
            if ((element.x !== undefined) && (element.y !== undefined)) {
                minValue = Math.min(minValue, element.x);
                minValue = Math.min(minValue, element.y);
                maxValue = Math.max(maxValue, element.x);
                maxValue = Math.max(maxValue, element.y);

                $.each(data.datatypes[categorySeries], function (i, v) {
                    // put the data in the correct buckets
                    if (v === element[categorySeries]) {
                        seriesData[v].values.push(element);
                    }
                });
            }
        });

        let cfMap = {};
        $.each(g.DATASETS[g.dataSetID]['covar.info'], function(idx, elem) {
            cfMap[elem['sample.column']] = elem['display.name'];
        });

        // convert from object, to array

        let series = [];
        let colors = ['#7cb5ec', '#ed3215', '#8085e9', '#f7a35c', '#90ed7d',
            '#e4d354', '#2b908f', '#f45b5b', '#91e8e1'];
        $.each(seriesData, function(key, val) {
            series.push({
                data: val.values,
                name: key,
                marker: {
                    radius: 2
                },
                type: 'scatter',
                color: colors[series.length % colors.length]
            });
        });

        minValue = Math.floor(minValue);
        maxValue = Math.ceil(maxValue);

        logDebug('minValue=', minValue);
        logDebug('maxValue=', maxValue);
        logDebug('series=', series);

        let yAxisTitle = data['id.correlate'];

        /*
        if (correlateSymbol !== '') {
            yAxisTitle = correlateSymbol;
        }
        */

        let exporting = appendExportButton('Download CSV', function() {
            exportCorrelationPlotData(data.id, data['id.correlate'], data.data);
        });
        exporting.filename = data.id + '_' + data['id.correlate'] + '_CORRELATION';

        g.chartCorrelation = Highcharts.chart({
            chart: {
                type: 'scatter',
                renderTo: 'plotCorrelation',
                zoomType: 'xy',
                height: 350,
                width: 350
            },
            exporting: exporting,
            plotOptions: {
                scatter: {
                    tooltip: {
                        headerFormat: null,
                        outside: true,
                        pointFormatter: function() {
                            let val = this.options;
                            let hoverText = `<b>${val['mouse.id']}</>`;

                            $.each(data.datatypes, function(i, e) {
                                hoverText += `<br><p>${cfMap[i]}: ${val[i]}</p>`;
                            });

                            return hoverText;
                        }
                    },
                    marker: {
                        radius: 5,
                        states: {
                            hover: {
                                enabled: true,
                                lineColor: 'rgb(100,100,100)'
                            }
                        }
                    },
                    states: {
                        hover: {
                            marker: {
                                enabled: false
                            }
                        }
                    },
                }
            },
            series: series,
            title: {
                text: '',
            },
            xAxis: {
                endOnTick: true,
                gridLineWidth: 1,
                //min: minValue,
                //max: maxValue,
                //tickAmount: 4,
                title: {
                    text: data.id
                },
            },
            yAxis: {
                endOnTick: true,
                gridLineWidth: 1,
                //min: minValue,
                //max: maxValue,
                //tickAmount: 4,
                title: {
                    text: yAxisTitle
                }
            },
        });

    }

    function updateCorrelationData(groupID, currentDataset, currentID, correlateDataset, correlateID, correlateSymbol) {
        // send GET request to status URL
        logDebug('updateCorrelationData');
        logDebug(currentDataset, currentID, correlateDataset, correlateID, correlateSymbol);

        if (g.runningTask) {
            let statusURL = 'http://ctlin0098.jax.org/';
            $.ajax({
                type: 'GET',
                url: statusURL,
                retries: 3,
                retryInterval: 1000,
                success: function (dataIn, status, request) {
                    var data = getCallGroupResponse(groupID)
                    logDebug('DATA=======', data);
                    if (data.status === 'DONE') {
                        if ('error' in data) {
                            // MAJOR ERROR
                            let message = `Unfortunately, there was a problem contacting the server.  Please try again.`;
                            stopTask();
                            showErrorMessage(message, null);
                        } else if (data.number_tasks_errors !== 0) {
                            let message = `Unfortunately, we encountered an error.  Please try again.`;
                            let errorMessages = '';
                            $.each(data.response_data, function (key, value) {
                                if ('error' in value) {
                                    errorMessages += (`<strong>${key}:</strong> ${value.error}<br>`);
                                }
                            });

                            stopTask();
                            showErrorMessage(message, errorMessages);
                        } else if (data.number_tasks_errors === 0) {
                            // check to make sure the status codes are good
                            let errorMessages = '';
                            $.each(data.response_data, function (key, value) {
                                if (value.status_code !== 200) {
                                    errorMessages += (`<strong>${key}:</strong> ${value.response.error}<br>`);
                                }
                            });

                            if (errorMessages !== '') {
                                let message = `Unfortunately, there was a problem calculating the Correlation Plot.`;
                                stopTask();
                                showErrorMessage(message, errorMessages);
                            } else {
                                g.correlationChartData = data.response_data.correlationPlot.response.result;
                                plotCorrelationChart();
                                stopTask();
                            }
                        }
                    } else {
                        // rerun in 1 seconds
                        logDebug('Not done, keep checking...');
                        setTimeout(function () {
                            updateCorrelationData(groupID, currentDataset, currentID, correlateDataset, correlateID, correlateSymbol);
                        }, 1000);  // TODO: change to 1000 (1 second)
                    }
                }
            });
        } else {
            // TODO: cleanup
            logDebug('canceling');
            let cancelURL = `/api/cancel/${groupID}`;
            $.getJSON(cancelURL, function (data) {
                logDebug(data);
                g.chartCorrelation = null;
                $('#correlationDataTable').html('');
                $('#plotCorrelation').html('');
            });
        }

    }

    function generateCorrelationPlot(currentDataset, currentID, correlateDataset, correlateID, correlateSymbol, interactiveCovariate) {
        logDebug('Generating correlation plot:', currentDataset, currentID, correlateDataset, correlateID, correlateSymbol);
//        let urlCorrelationPlot = `http://10.105.9.102/api/get/http://localhost:8001/correlationplot?dataset=${currentDataset}&id=${currentID}&dataset_correlate=${correlateDataset}&id_correlate=${correlateID}`;
        let urlCorrelationPlot = _API_R_BASE + 'correlationplot?dataset=' + currentDataset
            + '&id=' + currentID + '&dataset_correlate=' + correlateDataset + '&id_correlate= ' + correlateID;

        if ((interactiveCovariate !== undefined) &&
            (interactiveCovariate !== null) &&
            (interactiveCovariate !== 'none')) {
            //if (interactiveCovariate !== null) {
            urlCorrelationPlot += `&intcovar=${interactiveCovariate}`;
        }

        let submitData = {
            urls:[{
                url_id: 'correlationPlot',
                url: urlCorrelationPlot
            }]};

        startTask();

        // reset the chart and clear it
        g.correlationPlot = null;
        $('#plotCorrelation').html('');
// dddd

        var q = d3.queue();
        for (var i=0; i<submitData.urls.length; i++) {
            var url = submitData.urls[i];
            q.defer(downloadData2, url.url, url.url_id);
        }

        var group_id = newCallGroup(submitData.urls);
        q.awaitAll(function(error, responseList) {
            updateGroupResponse(group_id, error, responseList);
        	updateCorrelationData(group_id, currentDataset, currentID, correlateDataset, correlateID, correlateSymbol);
        	g.runningTask = true;
            logDebug('group_id=', group_id);
        });
/*
        $.ajax({
            type: 'POST',
            url: '/api/submit',
            contentType: 'application/json',
            data: JSON.stringify(submitData),
            retries: 3,
            retryInterval: 1000,
            success: function(data, status, request) {
                updateCorrelationData(data.group_id, currentDataset, currentID, correlateDataset, correlateID, correlateSymbol);
            },
            error: function(jqXHR, textStatus, errorThrown) {
                showErrorMessage(errorThrown, textStatus);
            }
        });
*/
    }


    function resetSecondaryPlots() {
        g.chartMediation = null;
        g.chartEffect = {};
        g.chartSNPAssocSNPS = null;
        g.chartSNPAssocGenes = null;

        $('#plotMediation').html('');
        $('#allEffectPlots').html('');
        $('#plotSNPAssocSNPS').html('');
        $('#plotSNPAssocGenes').html('');
        $('#snpWindowMenu').html('');
    }

    function updateGeneSelect(groupID, geneID, proteinID, dataSetID, covar) {
        // send GET request to status URL
        logDebug('update progress, calling .ajax');

        logDebug(groupID);
        if (g.runningTask) {
            let statusURL = 'http://ctlin0098.jax.org/';
            $.ajax({
                type: 'GET',
                url: statusURL,
                retries: 3,
                retryInterval: 1000,
                success: function (dataIn, status, request) {
                    var data = getCallGroupResponse(groupID)
                    logDebug('DATA=======', data);
                    if (data.status === 'DONE') {
                        if ('error' in data) {
                            // MAJOR ERROR
                            let message = `Unfortunately, there was a problem contacting the server.  Please try again.`;
                            stopTask();
                            showErrorMessage(message, null);
                        } else if (data.number_tasks_errors !== 0) {
                            let message = `Unfortunately, we encountered an error.  Please try again.`;
                            let errorMessages = '';
                            $.each(data.response_data, function (key, value) {
                                if ('error' in value) {
                                    errorMessages += (`<strong>${key}:</strong> ${value.error}<br>`);
                                }
                            });

                            stopTask();
                            showErrorMessage(message, errorMessages);
                        } else if (data.number_tasks_errors === 0) {
                            // check to make sure the status codes are good
                            let errorMessages = '';
                            $.each(data.response_data, function (key, value) {
                                if (value.status_code !== 200) {
                                    errorMessages += (`<strong>${key}:</strong> ${value.response.error}<br>`);
                                }
                            });

                            if (errorMessages !== '') {
                                let message = `Unfortunately, there was a problem calculating the LOD SCAN.`;
                                stopTask();
                                showErrorMessage(message, errorMessages);
                            } else {
                                // show result, there will be 3 datasets to get

                                // 1. gene information
                                g.gene = data.response_data.geneData.response;
                                displayGeneData(g.gene.gene);
                                let geneData = null;
                                for (let key in g.gene.gene) {
                                    geneData = g.gene.gene[key];
                                }
                                // 2. LOD score information
                                if (covar === 'additive') {
                                    g.LODChartData[covar] = data.response_data.lod.response.result;
                                    plotLODChart(data.response_data.lod.response.result, covar, null);
                                } else {
                                    g.LODChartData.additive = data.response_data.lod.response.result;
                                    g.LODChartData[covar] = data.response_data.lodCovar.response.result;

                                    plotLODChart(data.response_data.lodCovar.response.result,
                                                 covar,
                                                 null);

                                    plotLODChart(data.response_data.lodCovar.response.result,
                                                 covar,
                                                 data.response_data.lod.response.result);
                                }

                                $('#interactiveCovarLODS').selectpicker('val', covar);

                                // 3. Expression information
                                g.expressionData = data.response_data.expression.response.result;

                                if (g.DATASETS[g.dataSetID].datatype === 'mrna') {
                                    plotProfile(g.geneID);
                                } else if (g.DATASETS[g.dataSetID].datatype === 'protein') {
                                    plotProfile(g.proteinID);
                                } else {
                                    logDebug('ERROR');
                                }

                                // 4. Correlation
                                displayCorrelation(data.response_data.correlation.response.result);

                                // first time we need to hide jumbotron instructions
                                // it's ok to keep hiding
                                $('#divWelcomeMesage').html('');
                                $('#divLOD').removeClass('invisible');
                                $('#divItemInfo').removeClass('invisible');
                                $('#divSecondRow').removeClass('invisible');
                                $('#divProfileCorrelation').removeClass('invisible');
                                stopTask();
                            }
                        }
                    }
                    else {
                        // rerun in 1 seconds
                        logDebug('Not done, keep checking...');
                        setTimeout(function () {
                            updateGeneSelect(groupID, geneID, proteinID, dataSetID, covar);
                        }, 1000);
                    }
                }
            });
        } else {
            // TODO: cleanup
            logDebug('canceling');
            let cancelURL = `/api/cancel/${groupID}`;
            $.getJSON(cancelURL, function (data) {
                logDebug(data);
            });
        }
    }

    /**
         * Perform gene search.
CCCC
         */
    function selectPhenotype(phenotypeID, dataSetID, covar) {
//        let urlExpression = `http://10.105.9.102/api/get/http://localhost:8001/expression?dataset=${dataSetID}&id=${phenotypeID}`;
//        let urlLOD = `http://10.105.9.102/api/get/http://localhost:8001/lodscan?dataset=${dataSetID}&id=${phenotypeID}`;
//        let urlLODCovar = `http://10.105.9.102/api/get/http://localhost:8001/lodscan?dataset=${dataSetID}&id=${phenotypeID}`;
//        let urlCorrelation = `http://10.105.9.102/api/get/http://localhost:8001/correlation?dataset=${dataSetID}&id=${phenotypeID}`;
    	var dataSetsURL = _API_R_BASE + 'datasets'
    	var urlExpression = _API_R_BASE + 'expression?dataset=' + dataSetID + '&id=' + phenotypeID;
    	var urlLOD = _API_R_BASE + 'lodscan?dataset=' + dataSetID + '&id=' + phenotypeID;
    	var urlCorrelation = _API_R_BASE + 'correlation?dataset=' + dataSetID + '&id=' + phenotypeID;




        // TODO: make reset function
        g.gene = null;
        g.geneID = null;
        g.proteinID = null;
        g.phenotypeID = phenotypeID;
        g.secondaryPlotMarkerID = null;
        g.secondaryPlotChromosome = null;
        g.secondaryPlotLocation = null;
        g.secondaryPlotCovar = null;
        g.correlationData = null;
        g.correlationDatasetID = dataSetID;
        g.LODChartData = {};
        g.LODChartData.additive = null;

        // reset the covariate pickers
        configureCovarInfo(false);

        $.each(g.interactiveCovariates, function(idx, value) {
            g.LODChartData[value['column.name']] = null;
        });

        if (covar === 'additive') {
            urlLOD += '&intcovar=additive';
            urlLODCovar = null;
        } else {
            urlLOD += '&intcovar=additive';
            urlLODCovar += ('&intcovar=' + covar);
        }


        // TODO: add ncores?, regress_local? to urlLOD, or handle backend?
        //URL += '&ncores=' + _G.api_cores;

        if (urlLODCovar !== null) {
            urlLODCovar += '&cores=5';
        }
        urlLOD += '&cores=5';


        let submitData = {
            urls:[{
                url_id: 'expression',
                url: urlExpression
            }, {
                url_id: 'correlation',
                url: urlCorrelation
            }, {
                url_id: 'lod',
                url: urlLOD
            }]};

        if (urlLODCovar !== null) {
            submitData.urls.push({
                url_id: 'lodCovar',
                url: urlLODCovar
            });
        }

        logDebug('submitData=', submitData);

        startTask();

        $('#div_current_item_information_header').html('');
        $('#div_current_item_information_body').html('');

        g.chartLOD = null;
        g.chartLODCovariateFull = null;
        g.chartLODCovariateDiff = null;
        $('#lodPlotChart').html('');
        $('#lodPlotChartCovariateFull').html('');
        $('#lodPlotChartCovariateDiff').html('');

        g.chartCorrelation = null;
        $('#plotCorrelation').html('');
        $('#correlationDataTable').html('');


        // The following line fires a change event which we do not want
        // $('#correlationDatasetSelect').selectpicker('val', g.dataSetID);

        // Do this instead
        $('#correlationDatasetSelect').val(g.dataSetID);
        $('#correlationDatasetSelect').selectpicker('render');

        $('#fact-svg-chart').html('');

        resetSecondaryPlots();

// cccc
//    	var urlExpression = 'static/data/' + 'expression' + '.json';
//    	var urlLOD = 'static/data/' + 'lod' + '.json';
//    	var urlCorrelation = 'static/data/' + 'correlation' + '.json';

        var q = d3.queue();
        for (var i=0; i<submitData.urls.length; i++) {
            var url = submitData.urls[i];
            q.defer(downloadData2, url.url, url.url_id);
        }
        var group_id = newCallGroup(submitData.urls);
        q.awaitAll(function(error, responseList ) {
            updateGroupResponse(group_id, error, responseList);
        	updatePhenotypeSelect(group_id, dataSetID, covar);
        	g.runningTask = true;
            logDebug('group_id=', group_id);
        });
        /*
        $.ajax({
            type: 'POST',
            url: '/api/submit',
            contentType: 'application/json',
            data: JSON.stringify(submitData),
            retries: 3,
            retryInterval: 1000,
            success: function(data, status, request) {
                g.runningTask = true;
                logDebug('data=', data);
                logDebug('status=', status);
                logDebug('request=', request);
                //let status_url = request.getResponseHeader('Location');
                //logDebug('status_url=', status_url);
                logDebug('data.group_id=', data.group_id);
                updatePhenotypeSelect(data.group_id, dataSetID, covar);
            },
            error: function(jqXHR, textStatus, errorThrown) {
                showErrorMessage(errorThrown, textStatus);
            }
        });
        */
    }


// CCCC DEBUG
    function updatePhenotypeSelect(groupID, datasetID, covar) {
        // send GET request to status URL
        logDebug('update progress, calling .ajax');

        logDebug(groupID);

        if (g.runningTask) {
            let statusURL = 'http://ctlin0098.jax.org/';
            $.ajax({
                type: 'GET',
                url: statusURL,
                retries: 3,
                retryInterval: 1000,
                success: function (dataIn, status, request) {
                    var data = getCallGroupResponse(groupID)
                    logInfo(data);
                    logDebug('DATA=======', data);

                    if (data.status === 'DONE') {
                        if ('error' in data) {
                            // MAJOR ERROR
                            let message = `Unfortunately, there was a problem contacting the server.  Please try again.`;
                            stopTask();
                            showErrorMessage(message, null);
                        } else if (data.number_tasks_errors !== 0) {
                            let message = `Unfortunately, we encountered an error.  Please try again.`;
                            let errorMessages = '';
                            $.each(data.response_data, function (key, value) {
                                if ('error' in value) {
                                    errorMessages += (`<strong>${key}:</strong> ${value.error}<br>`);
                                }
                            });

                            stopTask();
                            showErrorMessage(message, errorMessages);
                        } else if (data.number_tasks_errors === 0) {
                            // check to make sure the status codes are good
                            let errorMessages = '';
                            $.each(data.response_data, function (key, value) {
                                if (value.status_code !== 200) {
                                    errorMessages += (`<strong>${key}:</strong> ${value.response.error}<br>`);
                                }
                            });

                            if (errorMessages !== '') {
                                let message = `Unfortunately, there was a problem calculating the LOD SCAN.`;
                                stopTask();
                                showErrorMessage(message, errorMessages);
                            } else {
                                // show result, there will be 2 datasets to get
                                displayPhenoData(g.phenotypeID);

                                // 1. LOD score information
                                //plotLODChart(data.response_data.lod.response.result);

                                if (covar === 'additive') {
                                    g.LODChartData[covar] = data.response_data.lod.response.result;
                                    plotLODChart(data.response_data.lod.response.result, covar, null);
                                } else {
                                    g.LODChartData.additive = data.response_data.lod.response.result;
                                    g.LODChartData[covar] = data.response_data.lodCovar.response.result;

                                    plotLODChart(data.response_data.lodCovar.response.result,
                                                 covar,
                                                 null);

                                    plotLODChart(data.response_data.lodCovar.response.result,
                                                 covar,
                                                 data.response_data.lod.response.result);
                                }

                                $('#interactiveCovarLODS').selectpicker('val', covar);

                                // 2. Correlation
                                displayCorrelation(data.response_data.correlation.response.result);

                                // 3. Expression information
                                g.expressionData = data.response_data.expression.response.result;
                                plotProfile(g.phenotypeID);

                                // first time we need to hide jumbotron instructions
                                // it's ok to keep hiding
                                $('#divWelcomeMesage').html('');
                                $('#divLOD').removeClass('invisible');
                                $('#divItemInfo').removeClass('invisible');
                                $('#divSecondRow').removeClass('invisible');
                                $('#divProfileCorrelation').removeClass('invisible');
                                stopTask();
                            }
                        }
                    }
                    else {
                        // rerun in 1 seconds
                        logDebug('Not done, keep checking...');
                        setTimeout(function () {
                            updatePhenotypeSelect(groupID, datasetID, covar);
                        }, 1000);  // TODO: change to 1000 (1 second)
                    }
                }
            });
        } else {
            // TODO: cleanup
            logDebug('canceling');
            let cancelURL = `/api/cancel/${groupID}`;
            $.getJSON(cancelURL, function (data) {
                logDebug(data);
            });
        }
    }

    function updatePhenotypeSelect2(covar, correlation, expression, lod, lodCovar) {

        // show result, there will be 2 datasets to get
        displayPhenoData(g.phenotypeID);

        // 1. LOD score information
        //plotLODChart(data.response_data.lod.response.result);

        if (covar === 'additive') {
            g.LODChartData[covar] = lod.result;
            plotLODChart(lod.result, covar, null);
        } else {
            g.LODChartData.additive = lod.result;
            g.LODChartData[covar] = lodCovar.result;

            plotLODChart(lodCovar.result,
                         covar,
                         null);

            plotLODChart(lodCovar.result,
                         covar,
                         lod.result);
        }

        $('#interactiveCovarLODS').selectpicker('val', covar);

        // 2. Correlation
        displayCorrelation(correlation.result);

        // 3. Expression information
        g.expressionData = expression.result;
        plotProfile(g.phenotypeID);

        // first time we need to hide jumbotron instructions
        // it's ok to keep hiding
        $('#divWelcomeMesage').html('');
        $('#divLOD').removeClass('invisible');
        $('#divItemInfo').removeClass('invisible');
        $('#divSecondRow').removeClass('invisible');
        $('#divProfileCorrelation').removeClass('invisible');
        stopTask();

    }

    function showErrorMessage(message, details) {
        let errorMessage = `<div class="row text-left"><div class="col"><span class="font-weight-bold text-danger">${message}</span></div></div>`;

        if (details !== null) {
            errorMessage += `<div class="row row-spacer"></div>
                             <div class="row text-left">
                                 <div class="col">
                                     <div id="errorDetailAccordion" data-children=".item">
                                         <div class="item">
                                             <a class="text-danger" data-toggle="collapse" data-parent="#errorDetailAccordion" href="#errorDetailAccordion1" role="button" aria-expanded="true" aria-controls="errorDetailAccordion1">
                                                 Details
                                             </a>
                                             <div id="errorDetailAccordion1" class="collapse" role="tabpanel">
                                                 <p class="mb-3">
                                                     ${details}
                                                 </p>
                                             </div>
                                         </div>
                                     </div>
                                 </div>
                             </div>`;
        }

        swal({
              allowEnterKey: false,
              allowEscapeKey: false,
              allowOutsideClick: false,
              buttonsStyling: false,
              html: errorMessage,
              showCancelButton: false,
              showCloseButton: true,
              showConfirmButton: false,
              title: '<i class="fas fa-exclamation-circle"></i> Error!',
        });
    }

    /**
     * Perform gene search.
     */
    function selectGeneProtein(geneID, proteinID, dataSetID, covar) {
        logDebug('selectGeneProtein(' + geneID + ', ' + proteinID + ', ' + dataSetID + ', ' + covar);

//        let urlGeneData = g.PROTOCOL + '//churchilllab.jax.org/ensimpl/api/gene/${geneID}?id=&species=${g.SPECIES_ID}';
//        let urlExpression = `http://10.105.9.102/api/get/http://localhost:8001/expression?dataset=${dataSetID}&id=`;
//        let urlLOD = `http://10.105.9.102/api/get/http://localhost:8001/lodscan?dataset=${dataSetID}&id=`;
//        let urlLODCovar = `http://10.105.9.102/api/get/http://localhost:8001/lodscan?dataset=${dataSetID}&id=`;
//        let urlCorrelation = `http://10.105.9.102/api/get/http://localhost:8001/correlation?dataset=${dataSetID}&id=`;

        let urlGeneData = 'http://ctlin0098.jax.org/ensimpl/api/gene/' + geneID + '?id=&species=' + g.SPECIES_ID;
        let urlExpression = _API_R_BASE + 'expression?dataset=' + dataSetID + '&id=';
        let urlLOD = _API_R_BASE + 'lodscan?dataset=' + dataSetID + '&id=';
        let urlLODCovar = _API_R_BASE + 'lodscan?dataset=' + dataSetID + '&id=';
        let urlCorrelation = _API_R_BASE + 'correlation?dataset=' + dataSetID + '&id=';

        if (g.ENSEMBL_RELEASE != null) {
            urlGeneData += '&release=' + g.ENSEMBL_RELEASE;
        }

        g.gene = null;
        g.geneID = geneID;
        g.proteinID = proteinID;
        g.phenotypeID = null;
        g.secondaryPlotMarkerID = null;
        g.secondaryPlotChromosome = null;
        g.secondaryPlotLocation = null;
        g.secondaryPlotCovar = null;
        g.correlationData = null;
        g.correlationDatasetID = dataSetID;
        g.LODChartData = {};
        g.LODChartData.additive = null;

        //$('#factorSeriesCorrelation').selectpicker();

        $.each(g.interactiveCovariates, function(idx, value) {
            g.LODChartData[value['column.name']] = null;
        });

        // reset the covariate pickers
        configureCovarInfo(false);

        if ((proteinID === undefined) || (proteinID === null)) {
            urlExpression += geneID;
            urlLOD += geneID;
            urlLODCovar += geneID;
            urlCorrelation += geneID;
        } else {
            urlExpression += proteinID;
            urlLOD += proteinID;
            urlLODCovar += proteinID;
            urlCorrelation += proteinID;
        }

        if (covar === 'additive') {
            urlLOD += '&intcovar=additive';
            urlLODCovar = null;
        } else {
            urlLOD += '&intcovar=additive';
            urlLODCovar += ('&intcovar=' + covar);
        }

        // TODO: add ncores?, regress_local? to urlLOD, or handle backend?
        //URL += '&ncores=' + _G.api_cores;

        if (urlLODCovar !== null) {
            urlLODCovar += '&cores=5';
        }
        urlLOD += '&cores=5';


        let submitData = {
            urls:[{
                url_id: 'geneData',
                url: urlGeneData,
                is_call_outside: true
            }, {
                url_id: 'expression',
                url: urlExpression
            }, {
                url_id: 'correlation',
                url: urlCorrelation
            }, {
                url_id: 'lod',
                url: urlLOD
            }]};

        if (urlLODCovar !== null) {
            submitData.urls.push({
                url_id: 'lodCovar',
                url: urlLODCovar
            });
        }

        logDebug('submitData=', submitData);

        startTask();

        $('#div_current_item_information_header').html('');
        $('#div_current_item_information_body').html('');

        g.chartLOD = null;
        g.chartLODCovariateFull = null;
        g.chartLODCovariateDiff = null;
        $('#lodPlotChart').html('');
        $('#lodPlotChartCovariateFull').html('');
        $('#lodPlotChartCovariateDiff').html('');

        g.chartCorrelation = null;
        $('#plotCorrelation').html('');
        $('#correlationDataTable').html('');

        // The following line fires a change event which we do not want
        // $('#correlationDatasetSelect').selectpicker('val', g.dataSetID);

        // Do this instead
        $('#correlationDatasetSelect').val(g.dataSetID);
        $('#correlationDatasetSelect').selectpicker('render');


        $('#fact-svg-chart').html('');

        resetSecondaryPlots();

        var q = d3.queue();
        for (var i=0; i<submitData.urls.length; i++) {
            var url = submitData.urls[i];
            if ( url.is_call_outside ) {
                q.defer(downloadData, url.url, url.url_id);
            } else {
                q.defer(downloadData2, url.url, url.url_id);
            }
        }
        var group_id = newCallGroup(submitData.urls);

        q.awaitAll(function(error, responsesList) {
            updateGroupResponse(group_id, error, responsesList);
            updateGeneSelect(group_id, geneID, proteinID, dataSetID, covar);
            g.runningTask = true;
            logDebug('group_id=', group_id);
        });

/*
        $.ajax({
            type: 'POST',
            url: '/api/submit',
            contentType: 'application/json',
            data: JSON.stringify(submitData),
            retries: 3,
            retryInterval: 1000,
            success: function(data, status, request) {
                g.runningTask = true;
                logDebug('data=', data);
                logDebug('status=', status);
                logDebug('request=', request);
                //let status_url = request.getResponseHeader('Location');
                //logDebug('status_url=', status_url);
                logDebug('data.group_id=', data.group_id);
                updateGeneSelect(data.group_id, geneID, proteinID, dataSetID, covar);
            },
            error: function(jqXHR, textStatus, errorThrown) {
                showErrorMessage(errorThrown, textStatus);
            }
        });
*/
    }

    function updateChangeCovariate(groupID, covar) {
        // send GET request to status URL
        logDebug('update progress, calling .ajax');

        logDebug(groupID);

        if (g.runningTask) {
//            let statusURL = `/api/status/${groupID}`;
            let statusURL = 'http://ctlin0098.jax.org/';
            $.ajax({
                type: 'GET',
                url: statusURL,
                retries: 3,
                retryInterval: 1000,
                success: function (dataIn, status, request) {
                    var data = getCallGroupResponse(groupID)
                    logDebug('DATA=======', data);

                    /* A response will be like this:

                       {
                        number_tasks_completed: 3,
                        number_tasks_errors: 2,
                        number_tasks_submitted: 3,
                        response_data: {
                            lod: {
                                error: "Connection Error"
                                error_message: "HTTPConnectionPool(host='myapiisawesome', port=8000): Max retries exceeded with url: /lodscan?dataset=dataset.islet.rnaseq&nCores=2&id=ENSMUSG00000023224 (Caused by NewConnectionError('<urllib3.connection.HTTPConnection object at 0x7f861126ce48>: Failed to establish a new connection: [Errno -5] No address associated with hostname',))"
                                url: "http://myapiisawesome:8000/lodscan?dataset=dataset.islet.rnaseq&nCores=2&id=ENSMUSG00000023224"
                                url_id: "lod"
                            }
                        },
                        status: "DONE",
                        task_id: "d3c0b971-9794-4ed7-80cb-046710d2f9f5",
                        error: 'UNKNOWN ERROR'   <--- ONLY WHEN AN ERROR
                       }
                    */

                    if (data.status === 'DONE') {
                        if ('error' in data) {
                            // MAJOR ERROR
                            let message = `Unfortunately, there was a problem contacting the server.  Please try again.`;
                            stopTask();
                            showErrorMessage(message, null);
                        } else if (data.number_tasks_errors !== 0) {
                            let message = `Unfortunately, we encountered an error.  Please try again.`;
                            let errorMessages = '';
                            $.each(data.response_data, function (key, value) {
                                if ('error' in value) {
                                    errorMessages += (`<strong>${key}:</strong> ${value.error}<br>`);
                                }
                            });

                            stopTask();
                            showErrorMessage(message, errorMessages);
                        } else if (data.number_tasks_errors === 0) {
                            // check to make sure the status codes are good
                            let errorMessages = '';
                            $.each(data.response_data, function (key, value) {
                                if (value.status_code !== 200) {
                                    errorMessages += (`<strong>${key}:</strong> ${value.response.error}<br>`);
                                }
                            });

                            if (errorMessages !== '') {
                                let message = `Unfortunately, there was a problem calculating the LOD SCAN.`;
                                stopTask();
                                showErrorMessage(message, errorMessages);
                            } else {
                                // show result, there will be 1
                                g.LODChartData[covar] = data.response_data.lod.response.result;

                                plotLODChart(data.response_data.lod.response.result,
                                             covar,
                                             null);

                                plotLODChart(data.response_data.lod.response.result,
                                             covar,
                                             g.LODChartData.additive);

                                stopTask();
                            }
                        }
                    }
                    else {
                        // rerun in 1 seconds
                        logDebug('Not done, keep checking...');
                        setTimeout(function () {
                            updateChangeCovariate(groupID, covar);
                        }, 1000);  // TODO: change to 1000 (1 second)
                    }
                }
            });
        } else {
            // TODO: cleanup
            logDebug('canceling');
            let cancelURL = `/api/cancel/${groupID}`;
            $.getJSON(cancelURL, function (data) {
                logDebug(data);
            });
        }
    }

    /**
     * Change covar LOD plot.
     */
     function changeCovariate(covariate) {
        logDebug('changeCovariate(', covariate, ')');

        if (covariate === 'additive') {
            // remove all other graphs
            logDebug('additive, clear others');
            g.chartLODCovariateFull = null;
            g.chartLODCovariateDiff = null;
            $('#lodPlotChartCovariateFull').html('');
            $('#lodPlotChartCovariateDiff').html('');
        } else {
            g.chartLODChart = null;
            g.chartLODCovariateFull = null;
            g.chartLODCovariateDiff = null;
            $('#lodPlotChart').html('');
            $('#lodPlotChartCovariateFull').html('');
            $('#lodPlotChartCovariateDiff').html('');
        }


        // check to see if we have the data, if so, plot it
        // else go get it
        if (covariate in g.LODChartData) {
            logDebug('covar in g.LODChartData, so just add it');
            if (covariate === 'additive') {
                plotLODChart(g.LODChartData[covariate], covariate, null);
            } else {
                plotLODChart(g.LODChartData[covariate], covariate, null);
                plotLODChart(g.LODChartData[covariate], covariate, g.LODChartData.additive);
            }
        } else {
            logDebug('covar NOT in g.LODChartData, so go get it');
//            let urlLOD = `http://10.105.9.102/api/get/http://localhost:8001/lodscan?dataset=${g.dataSetID}&id=`;
            let urlLOD = _API_R_BASE + 'lodscan?dataset=' + g.dataSetID + '&id=';

            g.secondaryPlotMarkerID = null;
            g.secondaryPlotChromosome = null;
            g.secondaryPlotLocation = null;
            g.secondaryPlotCovar = null;

            if (g.DATASETS[g.dataSetID].datatype === 'mrna') {
                urlLOD += `${g.geneID}`;
            } else if (g.DATASETS[g.dataSetID].datatype === 'protein') {
                urlLOD += `${g.proteinID}`;
            } else if (g.DATASETS[g.dataSetID].datatype === 'pheno') {
                urlLOD += `${g.phenotypeID}`;
            } else {
                // TODO: handle error
                logError('MAJOR PROBLEM');
            }

            urlLOD += ('&intcovar=' + covariate);

            // TODO: add ncores?, regress_local? to urlLOD, or handle backend?
            //URL += '&ncores=' + _G.api_cores;

            urlLOD += '&cores=5';


            let submitData = {
                urls:[{
                    url_id: 'lod',
                    url: urlLOD
                }]};

            startTask();

            $('#fact-svg-chart').html('');

            resetSecondaryPlots();

            var q = d3.queue();
            for (var i=0; i<submitData.urls.length; i++) {
                var url = submitData.urls[i];
                q.defer(downloadData2, url.url, url.url_id);
            }

            var group_id = newCallGroup(submitData.urls);
            q.awaitAll(function(error, responseList) {
                updateGroupResponse(group_id, error, responseList);
            	updateChangeCovariate(group_id, covariate);
            	g.runningTask = true;
                logDebug('group_id=', group_id);
            });
/*
            $.ajax({
                type: 'POST',
                url: '/api/submit',
                contentType: 'application/json',
                data: JSON.stringify(submitData),
                retries: 3,
                retryInterval: 1000,
                success: function(data, status, request) {
                    g.runningTask = true;
                    logDebug('data=', data);
                    logDebug('status=', status);
                    logDebug('request=', request);
                    //let status_url = request.getResponseHeader('Location');
                    //logDebug('status_url=', status_url);
                    logDebug('data.group_id=', data.group_id);
                    updateChangeCovariate(data.group_id, covariate);
                },
                error: function(jqXHR, textStatus, errorThrown) {
                    showErrorMessage(errorThrown, textStatus);
                }
            });
*/
        }

    }

    return {
        g: g,
        init: init,
        findGene: findGene,
        startTask: startTask,
        stopTask: stopTask
    }
}();


function init(data_api_url) {

	logInfo('init')

/*
	// switch to different R data server based on the parameter
	var urlParams = new URLSearchParams(window.location.search);
	data_api_url = urlParams.get('data_api_url');
	logInfo('data_api_url: ' + data_api_url)
	if ( data_api_url != null && data_api_url.length > 0 ) {
		_API_R_BASE = data_api_url;
	}
*/
    _API_R_BASE = data_api_url;
    if ( _API_R_BASE == null ) {
        _API_R_BASE = 'http://ctlin0098.jax.org/attie-islet-secretion/';
    }
	logInfo(_API_R_BASE);

    window.loading_screen = window.pleaseWait({
        logo: '',
        backgroundColor: '#000000',
        loadingHtml: '<div id="loader-wrapper"><div id="loader"></div><div class="loader-section section-left"></div><div class="loader-section section-right"></div></div>'
    });


    QTL.init();

    // handle search
    $('#searchTerm').keypress(function(evt) {
        let code = evt.which ? evt.which : evt.keyCode;
        if (code === 13) {
            $(this).blur();
            $('#btnGo').focus().click();
        }
    });

    $('#btnGo').button().on('click', function(evt) {
        evt.preventDefault();
        let term = $('#searchTerm');
        QTL.findGene(term.val().trim().toUpperCase());
        return false;
    });

    // set some global options for Highcharts
    Highcharts.setOptions({
        chart: {
            style: {
                fontFamily: $('body').css('font-family')
            },
        },

        credits: {
            enabled: false
        },

        exporting: {
            buttons: {
                contextButton: {
                    menuItems: [
                        'downloadPNG',
                        'downloadJPEG',
                        'downloadPDF',
                        'downloadSVG',
                        'separator'
                    ]
                }
            },
        },
    });


    $('a[data-toggle="tab"]').on('shown.bs.tab', function(e){
       $($.fn.dataTable.tables(true)).DataTable().columns.adjust();
    });
}

