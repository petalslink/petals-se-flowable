/**
 * Copyright (c) 2018-2023 Linagora
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

import static org.junit.Assert.assertEquals;

import java.net.URI;

import org.junit.Test;

/**
 * Unit tests of {@link InternalBPMNDefinitionURIBuilder}.
 * 
 * @author Christophe DENEUX - Linagora
 *
 */
public class InternalBPMNDefinitionURIBuilderTest {

    private static final String SU_NAME = "my-service-unit";

    private static final String BPMN_DEFINITION_SIMPLE_FILE = "bpmn-definition.xml.bpmn";

    private static final String BPMN_DEFINITION_FILE_WITH_DIR = "bpmn/bpmn-definition.xml.bpmn";

    @Test
    public void buildURI() {
        
        assertEquals(URI.create("petals:" + SU_NAME + ":" + BPMN_DEFINITION_SIMPLE_FILE),
                InternalBPMNDefinitionURIBuilder.buildURI(SU_NAME, BPMN_DEFINITION_SIMPLE_FILE));

        assertEquals(URI.create("petals:" + SU_NAME + ":" + BPMN_DEFINITION_FILE_WITH_DIR),
                InternalBPMNDefinitionURIBuilder.buildURI(SU_NAME, BPMN_DEFINITION_FILE_WITH_DIR));
    }

}
