/**
 * Copyright (c) 2015-2016 Linagora
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
package org.ow2.petals.activitibpmn.identity.file;

import org.activiti.engine.cfg.AbstractProcessEngineConfigurator;
import org.activiti.engine.cfg.ProcessEngineConfigurator;
import org.activiti.engine.impl.cfg.ProcessEngineConfigurationImpl;
import org.ow2.petals.activitibpmn.identity.IdentityService;

/**
 * A {@link ProcessEngineConfigurator} that integrates a Activiti process engine into a JBI component.
 * 
 * @author Christophe DENEUX - Linagora
 */
public class FileConfigurator extends AbstractProcessEngineConfigurator {

    private final IdentityService identityService;

    /**
     * @param identityService
     *            Identity service to use. Not <code>null</code>.
     */
    public FileConfigurator(final IdentityService identityService) {

        assert identityService != null : "identityService can not be null !!";

        this.identityService = identityService;
    }

    @Override
    public void beforeInit(final ProcessEngineConfigurationImpl processEngineConfiguration) {
        // Nothing to do
    }

    @Override
    public void configure(final ProcessEngineConfigurationImpl processEngineConfiguration) {
        processEngineConfiguration.getSessionFactories().put(
                this.identityService.getUserEntityManagerFactory().getSessionType(),
                this.identityService.getUserEntityManagerFactory());
        processEngineConfiguration.getSessionFactories().put(
                this.identityService.getGroupEntityManagerFactory().getSessionType(),
                this.identityService.getGroupEntityManagerFactory());
        processEngineConfiguration.getSessionFactories().put(
                this.identityService.getMembershipEntityManagerFactory().getSessionType(),
                this.identityService.getMembershipEntityManagerFactory());
    }

}
