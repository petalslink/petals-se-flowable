/**
 * Copyright (c) 2017 Linagora
 * 
 * This program/library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or (at your
 * option) any later version.
 * 
 * This program/library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
 * for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program/library; If not, see http://www.gnu.org/licenses/
 * for the GNU Lesser General Public License version 2.1.
 */
package org.ow2.petals.flowable.identity;

import java.io.File;
import java.util.logging.Logger;

public abstract class AbstractProcessEngineConfigurator
        extends org.flowable.engine.cfg.AbstractProcessEngineConfigurator implements SeFlowableIdmServiceConfigurator {

    /**
     * The configuration file of this identity management engine.
     */
    protected File configurationFile;

    protected Logger logger;

    public void setConfigurationFile(final File configurationFile) {
        this.configurationFile = configurationFile;
    }

    public void setLogger(final Logger logger) {
        this.logger = logger;
    }

}
