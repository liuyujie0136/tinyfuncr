#!/bin/bash
# 12/9/2024, Yujie Liu

## usage
if [[ $# -lt 3 ]]; then
    echo -e "#### Prepare Track Data for ggepitracks3 ####\nUsage: bash prepEpiData.sh <locusFile> <trackConf> <bsseqConf> [<outDir>=out]\nAuthor: Yujie Liu\nDate: 12/9/2024"
    exit 1
fi

# get parameters
locusFile=$1
trackConf=$2
bsseqConf=$3
outDir=${4:-out}
locusFilename=$(basename $locusFile | cut -d "." -f 1)

# extract track data
mkdir -p $outDir/tmp
while read -a info; do
    trackName=${info[0]}
    trackType=${info[1]}
    if [ $trackType = "BS-seq" ]; then
        ctype=${info[2]}
        cat $bsseqConf | awk -v trackName=$trackName '$1==trackName' >$outDir/tmp/tmpConf
        while read -a bsseq; do
            if [ $ctype = "All" ]; then
                trackTypes=(CG CHG CHH)
            else
                trackTypes=($ctype)
            fi
            declare -A trackFiles=(["CG"]=${bsseq[1]} ["CHG"]=${bsseq[2]} ["CHH"]=${bsseq[3]})
        done <$outDir/tmp/tmpConf
        rm -f $outDir/tmp/tmpConf
    else
        trackTypes=("Cov")
        declare -A trackFiles=(["Cov"]=${info[2]})
    fi

    for trackType in ${trackTypes[*]}; do
        trackFile=${trackFiles[$trackType]}
        trackFileBG=$trackFile
        trackFiletype=${trackFile##*.}
        isBW=$(echo $trackFiletype | grep -E "bw|bigWig|bigwig" | wc -l)
        if [ $isBW -eq 1 ]; then
            trackFileBG=$outDir/tmp/$trackName.bg
            bigWigToBedGraph $trackFile $trackFileBG
        fi
        bedtools intersect -a $locusFile -b $trackFileBG -wo | cut -f 4-8 | awk -v type=$trackType 'BEGIN{OFS="\t"}; {print $1,type,$3,$4,$5}' >>$outDir/tmp/$trackName.$locusFilename.inter
    done
done <$trackConf

echo prepEpiData DONE
exit 0
