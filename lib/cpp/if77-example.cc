/* -*- Mode:C++; c-file-style:"gnu"; indent-tabs-mode:nil; -*- */
/*
 * Copyright (c) 2020 Lawrence Livermore National Laboratory
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 2 as
 * published by the Free Software Foundation;
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * Author: Peter D. Barnes, Jr. <pdbarnes@llnl.gov>
 */

/**
 * \file
 * \ingroup propagation
 * Example use of if77::If77 propagation model.
 */

#include "if77.h"

#include <iostream>

int
main (int argc, char ** argv)
{
  using namespace if77;
  
  If77Config if77;

  // This is the configuration shown in docs/atoa.in

  // Card 1:
  if77.IK    = If77Config::IK_t::FT_NM;
  if77.IO    = If77Config::IO_t::POWER_DENSITY;
  if77.IJ    = If77Config::IJ_t::USE_ALT;
  if77.ILB   = If77Config::ILB_t::NO_LOBES;
  if77.KK    = If77Config::KK_t::INSTANT;
  if77.IA    = 21;
  if77.TT    = "VOR";
  if77.DMAX  = 260;
  if77.JC    = 0;
  if77.IGPH  = 0;

  // Card 2:
  if77.HLA   = 16;
  if77.IFA   = If77Config::IFA_t::LOOP_4;
  if77.JT    = If77Config::JT_t::DIRECTIVE;
  if77.IPL   = If77Config::IPL_t::HORIZONTAL;
  if77.T1T   = 0;
  if77.HLPBW = 0;
  if77.SUR   = 0;
  if77.IZ    = If77Config::IZ_t::NONE;
  if77.STS   = If77Config::STS_t::STORM_0KM;
  if77.KD    = If77Config::KD_t::SMOOTH;
  if77.KE    = If77Config::KE_t::NONE;
  if77.DHSI  = 0;
  if77.DHOI  = 0;
  if77.HHOI  = 0;
  if77.IDG   = 0;
  if77.IMN   = 0;
  if77.ISEC  = 0;
  if77.DCI   = 52;
  if77.HCI   = 12;
  if77.ICC   = If77Config::KSC_t::METALLIC;

  // Card 3:
  if77.HAI   = 3000;
  if77.IAA   = If77Config::IFA_t::ISOTROPIC;
  if77.JS    = If77Config::JT_t::DIRECTIVE;
  if77.NPL   = If77Config::IPL_t::HORIZONTAL;
  if77.T2T   = 0;
  if77.H2PBW = 0;
  if77.ENO   = 301;
  if77.F     = 113;;
  if77.EIRP  = 22.2;
  if77.HPFI  = 0;
  if77.KSC   = If77Config::KSC_t::AVG_GROUND;
  if77.TP    = If77Config::TP_t::TEMP_0C;
  if77.SCK   = 0;
  if77.ISS   = 0;
  if77.JM    = If77Config::JM_t::STANDARD;
  if77.IOS   = If77Config::IOS_t::IOS_0;
  if77.IPK   = If77Config::IPK_t::NONE;
  if77.JO    = If77Config::JO_t::NONE;
  if77.KLM   = If77Config::KLM_t::CONTINENTAL_ALL;
  if77.MX1   = 0;
  if77.KLM2  = If77Config::KLM_t::CONTINENTAL_ALL;
  if77.MX2   = 0;

  std::cout << "\nIF77 configuration:\n"
            << if77
            << std::endl;

  std::cout << "\nIF77 configuration, as Fortran input cards:\n"
            << if77.InputCards ()
            << std::endl;

  double range {200};

  std::cout << "\nGetting propagation loss value for range "
            << range
            << std::endl;

  double ldBw = if77.GetLossDbW (range);
  std::cout << "\nReceived propagation loss "
            << ldBw << " dbW"
            << std::endl;

  std::cout << "\nGenerating a table of propagation loss\n\n"
            <<"Range (km)    Loss (dBW)\n";
  
  for (double r = 10; r < 10000; r *= 1.2)
    {
      double ldBw = if77.GetLossDbW (r);
      std::cout << r << "\t" << ldBw << "\n";
    }
  std::cout << std::endl;

      

  return 0;
}

