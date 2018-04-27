/**
 * Copyright (c) 2018 Linagora
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
package org.ow2.petals.flowable.utils;

import java.net.URI;

/**
 * <p>
 * A builder building the URL used to name the BPMN definition file into Flowable database.
 * </p>
 * <p>
 * The URI format is '<code>petals:&lt;service-unit-name&gt;:&lt;bpmn-file&gt;</code>' where;
 * </p>
 * <ul>
 * <li><code>service-unit-name</code> is the unique name of the service unit embedding the BPMN definition file,</li>
 * <li><code>bpmn-file</code> is the BPMN definition file path relative to the root directory of the service unit.</li>
 * </ul>
 * 
 * @author Christophe DENEUX - Linagora
 *
 */
public class InternalBPMNDefinitionURIBuilder {

    public static final String INTERNAL_SCHEME = "petals";

    private static final String URL_PATTERN = "%s:%s:%s";
    
    private InternalBPMNDefinitionURIBuilder() {
        // Utility class --> No constructor
    }

    public static URI buildURI(final String serviceUnitName, final String bpmnDefinitionFile) {
        return URI.create(String.format(URL_PATTERN, INTERNAL_SCHEME, serviceUnitName, bpmnDefinitionFile));
    }

}
