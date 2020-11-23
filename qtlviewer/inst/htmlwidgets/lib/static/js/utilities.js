
/**
 * @fileoverview A collection of utility functions.
 */

/**
 * Simple compare function.
 * @param {object} a - first object
 * @param {number} a.x - first number
 * @param {object} b - second object
 * @param {number} b.x - second number
 * @return {number} 0 if equal, 1 if a.x > b.x, -1 if a.x < b.x
 */
function compareX(a, b) {
    if (a.x < b.x)
        return -1;
    if (a.x > b.x)
        return 1;
    return 0;
}

/**
 * Format Mbp to two decimal places.
 * @param {number} Mbp - the number to round
 * @return {string} Mbp formatted to two decimal places
 */
function formatMbp(Mbp) {
    return Number(Mbp).toFixed(2);
}


/**
 * Get a random integer between min and max.
 * @param {number} minn - the number to round
 * @param {number} maxx - the number to round
 * @return {number} a number between minn and maxx
 */
function getRandomInt(minn, maxx) {
    minn = Math.ceil(minn);
    maxx = Math.floor(maxx);
    return Math.floor(Math.random() * (maxx - minn + 1)) + minn;
}



/**
 * Permutate arrays.
 * Example:
 *     let p = permutateArrays(['1', '2', '3'], ['A', 'B']]);
 *     console.log(p);
 *     [['1', 'A'], ['1', 'B'],
 *      ['2', 'A'], ['2', 'B'],
 *      ['3', 'A'], ['3', 'B']]
 *
 * Adapted from:
 * https://stackoverflow.com/questions/15298912/javascript-generating-combinations-from-n-arrays-with-m-elements
 *
 * @param arraysToCombine {Array} - array of arrays
 * @returns {Array} permutated array
 */
function permutateArrays(arraysToCombine) {
    let divisors = [];
    for (let i = arraysToCombine.length - 1; i >= 0; i--) {
        divisors[i] = divisors[i + 1]
            ? divisors[i + 1] * arraysToCombine[i + 1].length
            : 1;
    }

    function getPermutation(n, arraysToCombine) {
        let result = [];
        let curArray;
        for (let i = 0; i < arraysToCombine.length; i++) {
            curArray = arraysToCombine[i];
            result.push(
                curArray[Math.floor(n / divisors[i]) % curArray.length]
            );
        }
        return result;
    }

    let numPerms = arraysToCombine[0].length;
    for (let i = 1; i < arraysToCombine.length; i++) {
        numPerms *= arraysToCombine[i].length;
    }

    let combinations = [];
    for (let i = 0; i < numPerms; i++) {
        combinations.push(getPermutation(i, arraysToCombine));
    }
    return combinations;
}


/**
 * Get the mean of an array of numbers.
 * @param data {Array} - array of numbers
 * @returns {number} mean of "data"
 */
function mean(data) {
    let len = data.length;
    let sum = 0;
    for (let i = 0; i < len; i++) {
        sum += parseFloat(data[i]);
    }
    return (sum / len);
}


/**
 * Because .sort() doesn't sort numbers correctly
 * @param a {number} - 1st value to compare
 * @param b {number} - 2nd value to compare
 * @returns {number} positive, negative, or zero
 */
function numSort(a, b) {
    return a - b;
}


/**
 * Get any percentile from an array.
 * @param data {Array} - array of numbers
 * @param percentile {number} - which percentile to get
 * @returns {number} the "percentile" of "data"
 */
function getPercentile(data, percentile) {
    data.sort(numSort);
    let index = (percentile/100) * data.length;
    let result;
    if (Math.floor(index) === index) {
         result = (data[(index-1)] + data[index])/2;
    }
    else {
        result = data[Math.floor(index)];
    }
    return result;
}


/**
 * Wrap the percentile calls in one method.
 * @param data {Array} - array of numbers
 * @returns {{low: number, q1: number, median: number, q3: number, high: number}}
 */
function getBoxValues(data) {
    let filteredData = [];

    $.each(data, function(idx, elem) {
        if (!isNaN(elem)) {
            filteredData.push(elem);
        }
    });

    return {low: Math.min.apply(Math, filteredData),
            q1: getPercentile(filteredData, 25),
            median: getPercentile(filteredData, 50),
            q3: getPercentile(filteredData, 75),
            high: Math.max.apply(Math, filteredData)};
}


/**************************************************************************
 **
 ** LAYOUT ALGORITHMS
 **
 *************************************************************************/


function createShelf(width) {
    return {
        width: width,
        free: width,
        used: 0,
        elements: []
    }
}

function addItems(shelves, items, width) {

    if (shelves.length === 0) {
        shelves.push(createShelf(width));
    }

    let spacing = 1000;
    //items.sort(function(a, b) {
    //    return (a.position_start - b.position_start);
    //});

    items.sort(function(a, b) {
        return (b.position_end - b.position_start) - (a.position_end - a.position_start);
    });

    //console.log('ADD ITEMS');
    $.each(items, function(i, item) {
        //console.log('ITEM=', item.name, item.position_end - item.position_start)
        let shelved = false;
        $.each(shelves, function(x, shelf) {

            if (shelf.elements.length === 0) {
                shelf.elements.push(item);
                shelf.free -= (item.end - item.start);
                shelf.used += (item.end - item.start);
                shelved = true;
                // break out of loop, go to next item
                return false;
            }

            // is there enough room?
            if (shelf.free > (item.end - item.start)) {
                // there is room, but can it fit in available slots?
                let canFit = true;
                $.each(shelf.elements, function(y, element) {
                    if (((item.start - spacing <= element.start) && (item.end + spacing >= element.start)) ||
                        ((item.start - spacing >= element.start) && (item.start - spacing <= element.end))) {
                        canFit = false;
                        // break out of loop
                        return false;
                    }
                });

                if (canFit) {
                    shelf.elements.push(item);
                    shelf.free -= (item.end - item.start);
                    shelf.used += (item.end - item.start);
                    shelved = true;
                }

                if (shelved) {
                    // break out of loop
                    return false;
                }
            }

        });
        if (!shelved) {
            let shelf = createShelf(width);
            shelf.elements.push(item);
            shelf.free -= (item.end - item.start);
            shelf.used += (item.end - item.start);
            shelves.push(shelf);
        }
    });


    return shelves;
}
