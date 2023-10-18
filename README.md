# scRNAseqWorkshop2023

This repo holds datasets and scripts that will be used during the practical component part of the 2023 CFAR bioinformatic analysis of single-cell RNA-Seq data workshop. We will try to have each folder updated the Sunday prior to each session. Please be sure to check the repo for updates as we will be adding content for later classes in the coming weeks.

The repo also features a 'discussion' section, where you should be able to ask questions and reference lines in the scripts if it's useful. 

## Setting up a project with Git

To download the scripts and data, you can use the standard Github workflow to clone the repo.
If you aren't familiar with Git, you can check out this [very thorough guide on using Git with Rstudio](https://happygitwithr.com/). 
You don't need to get that in depth for now, you can just get Git set up so you can create new 
projects from version controlled repos (like this one) and easily retreive new files as we add
them for subsequent sessions of the workshop. 

## Manually setting up a project

If you don't want to use the Git workflow, you can download a zip of the repo using the following steps.
Note that through this approach, you will have to manually re-download files as we add them to the repo. 

1) Hit the green 'Code' button in the top right to open a dropdown menu.  Select 'download zip' and save the compressed folder.
   
<img width="495" alt="image" src="https://github.com/yerkes-gencore/scRNAseqWorkshop2023/assets/46037149/2ba9f88c-81c5-439b-81fc-42d6c227e9ba">

2) Unzip the folder.
3) In Rstudio, select 'File' and 'New Project...'
   
   <img width="496" alt="image" src="https://github.com/yerkes-gencore/scRNAseqWorkshop2023/assets/46037149/b172c371-4bd3-44be-aab3-67b656cdf60f">

4) Navigate to the unziped repo folder to create the new project
   
   <img width="549" alt="image" src="https://github.com/yerkes-gencore/scRNAseqWorkshop2023/assets/46037149/350e709b-3379-40ca-bfbc-2f1247e68bfe">

If you've set up the project correctly, your files pane in Rstudio should look like this: a `.Rproj` file at the highest level of the repo.

<img width="129" alt="image" src="https://github.com/yerkes-gencore/scRNAseqWorkshop2023/assets/46037149/8bef303d-7d32-4c32-a640-ff6ba984a164">


The scripts rely on consistent filestructure in a project for the provided paths to work. If you save files in a format different than
how this repo is arranged, you will have to manually adjust filepaths. 

<img width="468" alt="These paths depend on the project structure" src="https://github.com/yerkes-gencore/scRNAseqWorkshop2023/assets/46037149/236ca130-9b41-4a36-81a3-9cf58dd11258">

