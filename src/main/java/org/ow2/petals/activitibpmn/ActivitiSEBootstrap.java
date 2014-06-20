/**
 * Copyright (c) 2014-2014 Linagora
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
 * along with this program/library; If not, see <http://www.gnu.org/licenses/>
 * for the GNU Lesser General Public License version 2.1.
 */
package org.ow2.petals.activitibpmn;

import java.util.ArrayList;
import java.util.List;

import org.ow2.petals.component.framework.DefaultBootstrap;

/**
 * The component class of the Activiti BPMN Service Engine.
 * @author Bertrand Escudie - Linagora
 */
public class ActivitiSEBootstrap extends DefaultBootstrap {

	
	 /*
     * (non-Javadoc)
     * 
     * @see
     * org.ow2.petals.component.framework.DefaultBootstrap#getAttributeList()
     */
    @Override
    public List<String> getAttributeList() {
        final List<String> attributes = new ArrayList<String>();
        
        attributes.add("httpPort");
        attributes.add("httpHostName");
        attributes.add("httpServicesList");
        attributes.add("httpServicesContext");
        attributes.add("httpServicesMapping");
        attributes.add("httpThreadPoolSizeMin");
        attributes.add("httpThreadPoolSizeMax");
        attributes.add("httpAcceptors");
        attributes.add("httpsEnabled");
        attributes.add("httpsPort");
        attributes.add("httpsKeystoreType");
        attributes.add("httpsKeystoreFile");
        attributes.add("httpsKeystorePassword");
        attributes.add("httpsKeyPassword");
        attributes.add("httpsTruststoreType");
        attributes.add("httpsTruststoreFile");
        attributes.add("httpsTruststorePassword");
        attributes.add("httpsClientAuthenticationEnabled");
        attributes.add("javaNamingFactoryInitial");
        attributes.add("javaNamingProviderURL");
        attributes.add("jmsConnectionFactoryJNDIName");

        return attributes;
    }
	
	public ActivitiSEBootstrap() {
		// TODO Auto-generated constructor stub
	}

}
