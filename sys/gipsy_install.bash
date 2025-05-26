#!/bin/bash

#-----------------------------------------------------------------------------
#  Check for absolute requirements.
#-----------------------------------------------------------------------------
fail=0

echo ""
which wget &> /dev/null
if [[ $? -ne 0 ]]; then
   echo "Non-interactive network downloader 'wget' not found!"
   fail=1
fi

which gfortran &> /dev/null
if [[ $? -ne 0 ]]; then
   echo "GNU Fortran 95 compiler 'gfortran' not found!"
   fail=1
fi

if [[ "${fail}" -ne 0 ]]; then
   echo ""
   echo "--- Cannot install GIPSY ---"
   exit 1
fi

pyfail=0

#-----------------------------------------------------------------------------
#  Check for Python requirements.
#-----------------------------------------------------------------------------
python -c "" &> /dev/null
if [[ $? -ne 0 ]]; then
   echo ""
   echo "No Python interpreter found."
   echo "GIPSY tasks written in Python will not be available."
   pyfail=1
fi

if [[ "${pyfail}" -eq 0 ]]; then
   python -c "import numpy" &> /dev/null
   if [[ $? -ne 0 ]]; then
      echo ""
      echo "Python package 'numpy' is missing."
      echo "GIPSY tasks written in Python will not be available."
      pyfail=1
   fi
fi

if [[ "${pyfail}" -eq 0 ]]; then

   python -c "import matplotlib" &> /dev/null
   if [[ $? -ne 0 ]]; then
      echo ""
      echo "Python package 'matplotlib' is missing."
      pyfail=1
   fi

   python -c "import vtk" &> /dev/null
   if [[ $? -ne 0 ]]; then
      echo ""
      echo "Python package 'vtk' is missing."
      echo "This currently only affects GIPSY task VTKVOLUME."
      pyfail=1
   fi

   python -c "import PyQt4" &> /dev/null
   if [[ $? -ne 0 ]]; then
      echo ""
      echo "Python package 'PyQt4' is missing."
      pyfail=1
   fi

   python -c "import pyfits" &> /dev/null
   if [[ $? -ne 0 ]]; then
      echo ""
      echo "Python package 'pyfits' is missing."
      pyfail=1
   fi
fi

if [[ "${pyfail}" -ne 0 ]]; then
   echo ""
   echo "Not all Python-related requirements are met."
   echo "You may proceed, but some or all Python tasks will be unavailable."
   echo "In principle, this can be corrected later."
   echo ""
fi

#-----------------------------------------------------------------------------
#  Ask user for GIPSY's location.
#-----------------------------------------------------------------------------
echo -n "Enter directory where to install GIPSY: "
read -r install_root

mkdir -p "${install_root}" &> /dev/null
cd "${install_root}"
install_root=$(pwd)
if [[ $? -ne 0 ]]; then
   echo ""
   echo ""
   echo "Cannot install in ${install_root}!"
   exit 1
fi

touch test.dum &> /dev/null
if [[ $? -ne 0 ]]; then
   echo ""
   echo ""
   echo "Cannot install GIPSY in ${install_root}!"
   exit 1
fi
rm test.dum

echo -n "OK to install GIPSY in ${install_root} ? [Y/N] "
read -r proceed
if [[ "${proceed}" != "Y" && "${proceed}" != "y" ]]; then
   echo ""
   echo ""
   echo "Installation cancelled."
   exit 1
fi

#-----------------------------------------------------------------------------
#  Start installation procedure.
#-----------------------------------------------------------------------------
logfile="${install_root}/install.log"
echo ""
echo "Log file will be written to ${logfile}."
echo "In case of problems, please check this file first and"
echo "attach it to your mail when you have to ask for help."
echo ""

export gip_root=$(pwd)
cd "${gip_root}"
mkdir import &> /dev/null
cd import
mkdir src &> /dev/null
cd src
echo "Fetching GIPSY distribution ..."
wget "ftp://ftp.astro.rug.nl/gipsy/src/gipsy_src.tar.gz" -o "${logfile}"
if [[ $? -ne 0 ]]; then
   echo ""
   echo ""
   echo "Failed to fetch GIPSY distribution!"
   exit 1
fi

cd ../..
tar xfz import/src/gipsy_src.tar.gz >> "${logfile}" 2>&1
cd "${gip_root}/sys"
./mkclient.csh 103 >> "${logfile}" 2>&1
mv clients.new "${gip_root}/loc/clients"
source "${gip_root}/sys/gipenv.sh"
# Set stack size for specific architectures
case "${gip_arch}" in
    alpha|convex|mips)
        echo "Setting stack size to unlimited for architecture: ${gip_arch}"
        ulimit -s unlimited
        ;;
    *)
        # Optional: echo "Architecture ${gip_arch} does not require specific stack size adjustments."
        ;;
esac
cd "${gip_loc}"
cp "${gip_sys}/setup.mgr" setup
cd "${gip_sys}"
./install.csh >> "${logfile}" 2>&1

if [[ $? -ne 0 ]]; then
   echo ""
   echo ""
   echo "Compiler setup failed!"
   exit 1
fi

cd "${gip_sys}"
./mkbookkeeper.csh >> "${logfile}" 2>&1
mv bookkeeper.new bookkeeper

echo "Start building GIPSY - wait ..."
p -update >> "${logfile}" 2>&1
p -rebuild ggivtk.src >> "${logfile}" 2>&1
./mkclient.csh 231 >> "${logfile}" 2>&1
mv clients.new "${gip_root}/loc/clients"
echo "+++ FINISHED +++"
