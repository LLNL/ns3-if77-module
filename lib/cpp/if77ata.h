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

#ifndef IF77ATA_H
#define IF77ATA_H


/**
 * \file
 * \ingroup propagation
 * IF77 ata.for common block declarations.
 */

/*
 * See
 * https://stackoverflow.com/questions/25846934/common-block-equivalent-in-c
 */

// gfortran names are lower case, trailing underscore
#define FREAD fread_
#define IF77ATA if77ata_
#define ATA ata_

namespace if77
{

  constexpr unsigned long CARDS_SIZE = 4 * 80;
  
}  // namespace if77


extern "C" {
  
  typedef struct {
    char CARDS[if77::CARDS_SIZE];
  } FREAD_COMMON;
  
  FREAD_COMMON FREAD;
  
  double IF77_ATA (const double * range);

  double ATA (const double * range);
  
}  // extern C


#endif  // IF77ATA_H
